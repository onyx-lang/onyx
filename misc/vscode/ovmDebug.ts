import {
	Logger, logger,
	LoggingDebugSession,
	InitializedEvent, TerminatedEvent, StoppedEvent, BreakpointEvent, OutputEvent,
	ProgressStartEvent, ProgressUpdateEvent, ProgressEndEvent, InvalidatedEvent,
	Thread, StackFrame, Scope, Source, Handles, Breakpoint, MemoryEvent, ThreadEvent, LoadedSourceEvent, Variable,
} from '@vscode/debugadapter';
import { DebugProtocol } from '@vscode/debugprotocol';
import EventEmitter = require('node:events');

import { Subject } from "await-notify";
import * as net from "node:net";
import * as child_process from "node:child_process";
import { ChildProcess } from 'node:child_process';
import * as fs from "node:fs";
import { LogOutputEvent, LogLevel } from '@vscode/debugadapter/lib/logger';
import { debug } from 'node:console';

interface IOVMAttachRequestArguments extends DebugProtocol.AttachRequestArguments {
    socketPath?: string;
    stopOnEntry?: boolean;
}

interface IOVMLaunchRequestArguments extends DebugProtocol.AttachRequestArguments {
    wasmFile?: string;
    onyxFiles?: [string];
	onyxPath: string;
	workingDir: string;
    stopOnEntry?: boolean;
}

interface IFrameReference {
	frameIndex: number;
	threadId: number;
}

interface IVariableReference {
	frameIndex: number;
	threadId: number;
	variableChain: number[];
	memoryLocation?: number;
}

export class OVMDebugSession extends LoggingDebugSession {

	private debugger: OVMDebugger;
	private running_process: ChildProcess;

	private _configurationDone: Subject = new Subject();
	private _clientConnectedNotifier: Subject = new Subject();
	private _clientConnected: boolean;

	private _loadedSources: Map<string, Source>;

	private _variableReferences = new Handles<IVariableReference>();
	private _frameReferences = new Handles<IFrameReference>();

	private stopOnEntry: boolean;

    public constructor() {
        super("ovm-debug-log.txt");

        this.setDebuggerLinesStartAt1(true);
        this.setDebuggerColumnsStartAt1(true);

		this._loadedSources = new Map();
		this._clientConnected = false;
				
        this.debugger = new OVMDebugger();

		this.debugger.on("breakpointHit", (ev) => {
			this.sendEvent(new StoppedEvent("breakpoint", ev.threadId));
		});

		this.debugger.on("paused", (ev) => {
			if (ev.reason == "entry") {
				this.sendEvent(new ThreadEvent("started", ev.thread_id));

				if (!this.stopOnEntry) {
					this.debugger.resume(ev.thread_id);
					return;
				}
			}

			this.sendEvent(new StoppedEvent(ev.reason, ev.threadId));
		});

		this.debugger.on("terminated", () => {
			this.sendEvent(new TerminatedEvent());
		});
    }

    protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {
        response.body = response.body || {};

        // the adapter implements the configurationDone request.
		response.body.supportsConfigurationDoneRequest = true;

		// make VS Code use 'evaluate' when hovering over source
		response.body.supportsEvaluateForHovers = false;

		// make VS Code show a 'step back' button
		response.body.supportsStepBack = false;

		// make VS Code support data breakpoints
		response.body.supportsDataBreakpoints = false;

		// make VS Code support completion in REPL
		response.body.supportsCompletionsRequest = false;
		// response.body.completionTriggerCharacters = [ ".", "[" ];

		// make VS Code send cancel request
		response.body.supportsCancelRequest = true;

		// make VS Code send the breakpointLocations request
		response.body.supportsBreakpointLocationsRequest = false;

		// make VS Code provide "Step in Target" functionality
		response.body.supportsStepInTargetsRequest = false;

		// the adapter defines two exceptions filters, one with support for conditions.
		response.body.supportsExceptionFilterOptions = false;
		/*response.body.exceptionBreakpointFilters = [
			{
				filter: 'namedException',
				label: "Named Exception",
				description: `Break on named exceptions. Enter the exception's name as the Condition.`,
				default: false,
				supportsCondition: true,
				conditionDescription: `Enter the exception's name`
			},
			{
				filter: 'otherExceptions',
				label: "Other Exceptions",
				description: 'This is a other exception',
				default: true,
				supportsCondition: false
			}
		];*/

		// make VS Code send exceptionInfo request
		response.body.supportsExceptionInfoRequest = false;

		// make VS Code send setVariable request
		response.body.supportsSetVariable = false;

		// make VS Code send setExpression request
		response.body.supportsSetExpression = false;

		// make VS Code send disassemble request
		response.body.supportsDisassembleRequest = true;
		response.body.supportsSteppingGranularity = true;
		response.body.supportsInstructionBreakpoints = false;

		// make VS Code able to read and write variable memory
		response.body.supportsReadMemoryRequest = true;
		response.body.supportsWriteMemoryRequest = true;

		response.body.supportSuspendDebuggee = false;
		response.body.supportTerminateDebuggee = true;
		response.body.supportsFunctionBreakpoints = true;
		response.body.supportsSingleThreadExecutionRequests = true;

		this.sendResponse(response);

		// since this debug adapter can accept configuration requests like 'setBreakpoint' at any time,
		// we request them early by sending an 'initializeRequest' to the frontend.
		// The frontend will end the configuration sequence by calling 'configurationDone' request.
		this.sendEvent(new InitializedEvent());
    }

    protected configurationDoneRequest(response: DebugProtocol.ConfigurationDoneResponse, args: DebugProtocol.ConfigurationDoneArguments, request?: DebugProtocol.Request): void {
        super.configurationDoneRequest(response, args);

		this._configurationDone.notify();
    }

	protected disconnectRequest(response: DebugProtocol.DisconnectResponse, args: DebugProtocol.DisconnectArguments, request?: DebugProtocol.Request): void {
		if (args.terminateDebuggee) {
			if (this.running_process) {
				this.running_process.kill('SIGTERM');
			}
		}

		this.sendResponse(response);
	}

    protected cancelRequest(response: DebugProtocol.CancelResponse, args: DebugProtocol.CancelArguments, request?: DebugProtocol.Request): void {
		this.sendResponse(response);
    }

	protected async setBreakPointsRequest(response: DebugProtocol.SetBreakpointsResponse, args: DebugProtocol.SetBreakpointsArguments, request?: DebugProtocol.Request): Promise<void> {
		while (!this._clientConnected) {
			await this._clientConnectedNotifier.wait();
		}

		const path = args.source.path;
		const clientLines = args.lines || [];

		await this.debugger.remove_breakpoints_in_file(path);

		const actualBreakpointsPromise = clientLines.map(async line => {
			const res = await this.debugger.set_breakpoint(path, line);
			const bp = new Breakpoint(res.verified, this.convertDebuggerLineToClient(res.line)) as DebugProtocol.Breakpoint;
			bp.id = res.id;
			return bp;
		});

		const actualBreakpoints = await Promise.all<DebugProtocol.Breakpoint>(actualBreakpointsPromise);

		response.body = {
			breakpoints: actualBreakpoints
		};
		this.sendResponse(response);
	}

	protected async stackTraceRequest(response: DebugProtocol.StackTraceResponse, args: DebugProtocol.StackTraceArguments, request?: DebugProtocol.Request): Promise<void> {
		let frames = await this.debugger.trace(args.threadId);

		this._frameReferences.reset();
		this._variableReferences.reset();

		response.body = {
			stackFrames: frames.map((f, i) => {
				let source = this.loadSource(f.filename);

				let frameRef = this._frameReferences.create({
					threadId: args.threadId,
					frameIndex: i
				});

				let stack_frame = new StackFrame(frameRef, f.funcname, source, f.line);
				stack_frame.instructionPointerReference = f.instructionPointer.toString();
				
				return stack_frame;
			})
		};

		this.sendResponse(response);
	}

	protected async threadsRequest(response: DebugProtocol.ThreadsResponse, request?: DebugProtocol.Request): Promise<void> {
		let threads = await this.debugger.threads();

		response.body = {
			threads: threads.map(t => new Thread(t.id, t.name))
		};

		this.sendResponse(response);
	}

	protected scopesRequest(response: DebugProtocol.ScopesResponse, args: DebugProtocol.ScopesArguments, request?: DebugProtocol.Request): void {
		let frameId = args.frameId;

		let frameRef = this._frameReferences.get(frameId);
		let varRef   = this._variableReferences.create({
			...frameRef,
			variableChain: []
		});

		response.body = {
			scopes: [
				new Scope("Locals", varRef, false),
			]
		};
		this.sendResponse(response);	
	}

	protected async variablesRequest(response: DebugProtocol.VariablesResponse, args: DebugProtocol.VariablesArguments, request?: DebugProtocol.Request): Promise<void> {
		const frameRef = this._variableReferences.get(args.variablesReference, {frameIndex:0, threadId:0, variableChain:[]});

		let vs: Variable[] = (await this.debugger.variables(frameRef.frameIndex, frameRef.threadId, frameRef.variableChain))
			.map(v => {
				let nv = new Variable(v.name, v.value) as DebugProtocol.Variable;
				nv.type = v.type;
				if (v.hasChildren) {
					nv.variablesReference = this._variableReferences.create({
						...frameRef,
						variableChain: frameRef.variableChain.concat([v.symId]),
					});
				}

				if (v.memoryReference > 0) {
					nv.memoryReference = v.memoryReference.toString();
				}

				return nv;
			});

		response.body = { variables: vs };
		this.sendResponse(response);
	}

    protected async launchRequest(response: DebugProtocol.LaunchResponse, args: IOVMLaunchRequestArguments, request?: DebugProtocol.Request): Promise<void> {
		let debugSocketPath = "/tmp/onyx-debug-socket";
		if (fs.existsSync(debugSocketPath)) {
			fs.unlinkSync(debugSocketPath);
		}

		this.sendEvent(new LogOutputEvent(`Spawning Onyx debug session\nSocket: ${debugSocketPath}\nWorking Dir: ${args.workingDir}\n`, LogLevel.Log));

		let onyx_path = `${args.onyxPath}/bin/onyx`

		if (args.wasmFile) {
			this.running_process = child_process.spawn(onyx_path, ["run", "--debug", "--debug-socket", debugSocketPath, args.wasmFile], {
				"cwd": args.workingDir,	
				"env": {
					"ONYX_PATH": args.onyxPath,
				}
			});

		} else if (args.onyxFiles) {
			this.running_process = child_process.spawn(onyx_path, ["run", "--debug",  "--debug-socket", debugSocketPath,...args.onyxFiles], {
				"cwd": args.workingDir,
				"env": {
					"ONYX_PATH": args.onyxPath,
				}
			});

		} else {
			this.sendErrorResponse(response, {
				format: "Expected either wasmFile or onyxFiles in launch configuration.",
				id: 1
			} as DebugProtocol.Message);

			return;
		}

		if (!this.running_process.pid) {
			this.sendErrorResponse(response, {
				format: "Failed to spawn Onyx debug session.",
				id: 1
			} as DebugProtocol.Message);

			return;
		}

		this.running_process.stdout.setEncoding("utf-8");
		this.running_process.stdout.on("data", (chunk) => {
			this.sendEvent(new OutputEvent(chunk, "console"));
		});

		this.sendEvent(new LogOutputEvent(`Process is spawned: ${this.running_process.pid}\n`, LogLevel.Log));

		let done_hack = false;

		let success = Promise.race([
			new Promise((res, rej) => setTimeout(() => res(false), 2000)),
			new Promise(async (res, rej) => {
				while (!done_hack) {
					if (fs.existsSync(debugSocketPath)) {
						res(true);
					}

					await new Promise((res, rej) => setTimeout(res, 100));
				}
			})
		])

		done_hack = true;

		if (!success) {
			this.sendErrorResponse(response, {
				format: "Failed to spawn Onyx debug session.",
				id: 1
			} as DebugProtocol.Message);
			return;
		}

		// This "sleep" is very hacky and needs to be replaced. The problem
		// is the we need to wait until the socket exists.

		await this.attachRequest(response, {"socketPath": debugSocketPath, "stopOnEntry": args.stopOnEntry});
    }

    protected async attachRequest(response: DebugProtocol.AttachResponse, args: IOVMAttachRequestArguments, request?: DebugProtocol.Request): Promise<void> {
		this.sendEvent(new LogOutputEvent(`Connecting to process\n`, LogLevel.Log));
		await this._configurationDone.wait(1000);

		try {
			await this.debugger.connect(args.socketPath);
		} catch (e) {
			this.sendEvent(new LogOutputEvent(`Error connecting to session: ${e.toString()}`, LogLevel.Error));
			this.sendErrorResponse(response, 41);
			return;
		}

		this.sendEvent(new LogOutputEvent(`Connected to process`, LogLevel.Log));

		this._clientConnected = true;
		this._clientConnectedNotifier.notify();

		this.stopOnEntry = args.stopOnEntry ?? false;

		this.sendResponse(response);
		// this.sendEvent(new ThreadEvent("started", 1));
    }

    protected pauseRequest(response: DebugProtocol.PauseResponse, args: DebugProtocol.PauseArguments, request?: DebugProtocol.Request): void {
    	this.debugger.pause(args.threadId);
    	this.sendResponse(response);
    }

	protected continueRequest(response: DebugProtocol.ContinueResponse, args: DebugProtocol.ContinueArguments, request?: DebugProtocol.Request): void {
		let thread_id = args.threadId;
		if (!args.singleThread) {
			thread_id = 0xffffffff;
		}

		response.body = {
			allThreadsContinued: !!args.singleThread
		};

		this.debugger.resume(thread_id);
		this.sendResponse(response);
	}

	protected nextRequest(response: DebugProtocol.NextResponse, args: DebugProtocol.NextArguments, request?: DebugProtocol.Request): void {
		this.debugger.step("over", args.threadId);
		this.sendResponse(response);
	}

	protected stepOutRequest(response: DebugProtocol.StepOutResponse, args: DebugProtocol.StepOutArguments, request?: DebugProtocol.Request): void {
		this.debugger.step("out", args.threadId);
		this.sendResponse(response);
	}
	
	protected stepInRequest(response: DebugProtocol.StepInResponse, args: DebugProtocol.StepInArguments, request?: DebugProtocol.Request): void {
		this.debugger.step("line", args.threadId);
		this.sendResponse(response);
	}

	protected async disassembleRequest(response: DebugProtocol.DisassembleResponse, args: DebugProtocol.DisassembleArguments, request?: DebugProtocol.Request): Promise<void> {
		let addr = parseInt(args.memoryReference) + args.offset;
		let instrs = await this.debugger.disassemble(addr, args.instructionCount);

		response.body = {
			instructions: instrs.map((i, index) => {
				let source: DebugProtocol.Source | null;
				if (i.newSource != null) {
					source = this.loadSource(i.newSource);
				}

				return {
					address: (index + addr).toString(),
					instruction: i.instr,
					line: i.line,
					location: source,
				};
			})
		};

		this.sendResponse(response);
	}

	protected async readMemoryRequest(response: DebugProtocol.ReadMemoryResponse, args: DebugProtocol.ReadMemoryArguments, request?: DebugProtocol.Request): Promise<void> {
		let addr = parseInt(args.memoryReference) + args.offset;

		let mem = await this.debugger.read_memory(addr, args.count);

		response.body = {
			address: addr.toString(),
			data: Buffer.from(mem.data).toString("base64"),
		};

		this.sendResponse(response);
	}

	protected async writeMemoryRequest(response: DebugProtocol.WriteMemoryResponse, args: DebugProtocol.WriteMemoryArguments, request?: DebugProtocol.Request): Promise<void> {
		let addr = parseInt(args.memoryReference) + args.offset;

		let data = Buffer.from(args.data, "base64");

		let bytesWritten = await this.debugger.write_memory(addr, data);

		response.body = {
			bytesWritten: bytesWritten,
		};

		this.sendResponse(response);
	}

	private fileNameToShortName(filename: string): string {
		return filename.substring(filename.lastIndexOf("/") + 1);
	}

	private loadSource(filename: string): Source {
		let source = new Source(
			this.fileNameToShortName(filename),
			this.convertDebuggerPathToClient(filename),
			undefined, undefined, "ovm-debug-src"
		);

		if (!this._loadedSources.has(source.name)) {
			this._loadedSources.set(source.name, source);

			this.sendEvent(new LoadedSourceEvent("new", source));
		}
				
		return source;
	}
}

interface IFileLocation {
	funcname: string;
	filename: string;
	line: number;
	instructionPointer: number;
}

interface IBreakpointValidation {
	verified: boolean;
	id: number;
	line: number;
}

interface IThreadInfo {
	id: number;
	name: string;
}

interface IVariableInfo {
	name: string;
	value: string;
	type: string;
	symId: number;
	hasChildren: boolean;
	memoryReference: number;
}

interface IReadMemory {
	data: ArrayBuffer;
}

interface IDisassembledInstruction {
	instr: string;
	line: number;
	newSource?: string;
}

enum OVMCommand {
	NOP     = 0,
	RES     = 1,
	PAUSE   = 2,
	BRK     = 3,
	CLR_BRK = 4,
	STEP    = 5,
	TRACE   = 6,
	THREADS = 7,
	VARS    = 8,
	MEM_R   = 9,
	MEM_W   = 10,
	DISASM  = 11
}

enum OVMEvent {
	NOP = 0,
	BREAKPOINT_HIT = 1,
	PAUSED = 2,
	RESPONSE = 0xffffffff
}

class OVMDebugger extends EventEmitter {

	private client: net.Socket;
	private pending_responses: any;

	private _next_cmd_id: number;
	private _promiseResolution: Map<number, (arg0: any) => void>;

	constructor() {
		super();
		this._promiseResolution = new Map();
	}

	connect(path: string): Promise<void> {
        this._next_cmd_id = 1;
		this.pending_responses = {};

		this.client = net.connect(path);

		this.client.on("data", this.parseIncoming.bind(this));

		this.client.on("end", () => {
			this.sendEvent("terminated");
		});

		return new Promise((res, rej) => {
			this.client.on("connect", res);
			this.client.on("error", rej);
		});
	}

	pause(thread_id: number = 0xffffffff): void {
		if (this.client == null) return;

        let data = new ArrayBuffer(12);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.PAUSE, true);
        view.setUint32(8, thread_id, true);

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.PAUSE;
	}

    resume(thread_id: number = 0xffffffff): void {
		if (this.client == null) return;

        let data = new ArrayBuffer(12);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.RES, true);
        view.setUint32(8, thread_id, true);

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.RES;
    }

    async set_breakpoint(filename: string, line: number): Promise<IBreakpointValidation> {
		if (this.client == null) return Promise.resolve({} as IBreakpointValidation);

        let data = new ArrayBuffer(16+filename.length);
        let view = new DataView(data);

		let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.BRK, true);

        view.setUint32(8, filename.length, true);
        for (let i=0; i<filename.length; i++) {
            view.setUint8(i+12, filename.charCodeAt(i));
        }
        view.setUint32(12+filename.length, line, true);

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.BRK;

		return this.preparePromise(cmd_id);
    }
	
	async remove_breakpoints_in_file(filename: string): Promise<boolean> {
		if (this.client == null) return Promise.resolve(false);

        let data = new ArrayBuffer(12+filename.length);
        let view = new DataView(data);

		let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.CLR_BRK, true);

        view.setUint32(8, filename.length, true);
        for (let i=0; i<filename.length; i++) {
            view.setUint8(i+12, filename.charCodeAt(i));
        }

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.CLR_BRK;

		return this.preparePromise(cmd_id);
	}

	step(granularity: "line" | "instruction" | "over" | "out", thread_id: number): void {
		if (this.client == null) return;

        let data = new ArrayBuffer(16);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.STEP, true);
        view.setUint32(12, thread_id, true);

		switch (granularity) {
			case "line":        view.setUint32(8, 1, true); break;
			case "instruction": view.setUint32(8, 2, true); break;
			case "over":        view.setUint32(8, 3, true); break;
			case "out":         view.setUint32(8, 4, true); break;
		}

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.STEP;
	}

	trace(thread_id: number): Promise<IFileLocation[]> {
		if (this.client == null) return Promise.resolve([]);

        let data = new ArrayBuffer(12);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.TRACE, true);
        view.setUint32(8, thread_id, true);

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.TRACE;

		return this.preparePromise(cmd_id);
	}

	threads(): Promise<IThreadInfo[]> {
		if (this.client == null) return Promise.resolve([]);

        let data = new ArrayBuffer(8);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.THREADS, true);

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.THREADS;

		return this.preparePromise(cmd_id);
	}

	variables(frame_index: number, thread_id: number, descention: number[]): Promise<IVariableInfo[]> {
		if (this.client == null) return Promise.resolve([]);

        let data = new ArrayBuffer(20 + 4 * descention.length);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.VARS, true);
        view.setUint32(8, frame_index, true);
        view.setUint32(12, thread_id, true);
		view.setUint32(16, descention.length, true);

		for (let i=0; i<descention.length; i++) {
			view.setUint32(20 + i * 4, descention[i], true);
		}

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.VARS;

		return this.preparePromise(cmd_id);
	}

	read_memory(addr: number, count: number): Promise<IReadMemory> {
		if (this.client == null) return Promise.resolve({} as IReadMemory);

        let data = new ArrayBuffer(16);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.MEM_R, true);
        view.setUint32(8, addr, true);
        view.setUint32(12, count, true);

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.MEM_R;

		return this.preparePromise(cmd_id);
	}

	write_memory(addr: number, data_to_write: ArrayBuffer): Promise<number> {
		if (this.client == null) return Promise.resolve(0);

        let data = new ArrayBuffer(16 + data_to_write.byteLength);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.MEM_W, true);
        view.setUint32(8, addr, true);
        view.setUint32(12, data_to_write.byteLength, true);

		new Uint8Array(data).set(new Uint8Array(data_to_write), 16);

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.MEM_W;

		return this.preparePromise(cmd_id);
	}

	disassemble(addr: number, count: number): Promise<IDisassembledInstruction[]> {
		if (this.client == null) return Promise.resolve([]);

        let data = new ArrayBuffer(16);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.DISASM, true);
        view.setUint32(8, addr, true);
        view.setUint32(12, count, true);

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.DISASM;

		return this.preparePromise(cmd_id);
	}

	private parseIncoming(data: Buffer): void {
		let parser = new DataParser(data);

		while (parser.offset != data.length) {
			let event_id = parser.parseUint32();

			switch (event_id) {
				case OVMEvent.NOP: break;

				case OVMEvent.BREAKPOINT_HIT: {
					let bp_id = parser.parseUint32();
					let thread_id = parser.parseUint32();

					this.sendEvent("breakpointHit", {
						breakpointId: bp_id,
						threadId: thread_id,
					});
					break;
				}

				case OVMEvent.PAUSED: {
					let thread_id = parser.parseUint32();
					let reason_id = parser.parseUint32();

					let reason = "unknown";
					switch (reason_id) {
						case 1: reason = "entry"; break;
						case 2: reason = "step"; break;
						case 3: reason = "exception"; break;
					}

					this.sendEvent("paused", { reason, threadId: thread_id });
					break;
				}

				case OVMEvent.RESPONSE: {
					this.handleResponse(parser);
					break;
				}

				default:
					// console.log("Unknown event: ", event_id, data);
			}
		}
	}

	private handleResponse(parser: DataParser) {
		let msg_id = parser.parseUint32();
		let cmd_id = this.pending_responses[msg_id] || OVMCommand.NOP;

		delete this.pending_responses[msg_id];

		switch (cmd_id) {
			case OVMCommand.NOP: break;
			case OVMCommand.RES: {
				let success = parser.parseBool();
				break;
			}

			case OVMCommand.BRK: {
				let success = parser.parseBool();
				let bp_id   = parser.parseInt32();
				let line    = parser.parseInt32();

				this.resolvePromise(msg_id, {
					verified: success,
					id: bp_id,
					line: line
				});
				break;
			}

			case OVMCommand.CLR_BRK: {
				let success = parser.parseBool();

				this.resolvePromise(msg_id, success);
				break;
			}

			case OVMCommand.STEP: break;

			case OVMCommand.TRACE: {
				let result = new Array<IFileLocation>();

				let count = parser.parseUint32();
				for (let i = 0; i < count; i++) {
					let funcname = parser.parseString();
					let filename = parser.parseString();
					let line     = parser.parseInt32();
					let ip       = parser.parseInt32();

					result.push({funcname, filename, line, instructionPointer: ip});
				}
				
				this.resolvePromise(msg_id, result);
				break;
			}

			case OVMCommand.THREADS: {
				let result = new Array<IThreadInfo>();

				let count = parser.parseUint32();
				for (let i = 0; i < count; i++) {
					let id   = parser.parseUint32();
					let name = parser.parseString();
					result.push({id, name});
				}

				this.resolvePromise(msg_id, result);
				break;
			}

			case OVMCommand.VARS: {
				let result = new Array<IVariableInfo>();

				while (parser.parseInt32() == 0) {
					let name  = parser.parseString();
					let value = parser.parseString();
					let type  = parser.parseString();
					let symId    = parser.parseUint32();
					let hasChildren = parser.parseBool();
					let memoryReference = parser.parseUint32();
					result.push({name, value, type, symId, hasChildren, memoryReference});
				}

				this.resolvePromise(msg_id, result);
				break;
			}

			case OVMCommand.MEM_R: {
				let data = parser.parseBytes();

				this.resolvePromise(msg_id, { data: data });
				break;
			}

			case OVMCommand.MEM_W: {
				let count = parser.parseUint32();

				this.resolvePromise(msg_id, count);
				break;
			}

			case OVMCommand.DISASM: {
				let result = new Array<IDisassembledInstruction>();

				while (parser.parseInt32() == 0) {
					let instr = parser.parseString();
					let line  = parser.parseUint32();

					let newSource: string | null = null;
					if (parser.parseBool()) {
						newSource = parser.parseString();
					}

					result.push({ instr, line, newSource });
				}

				this.resolvePromise(msg_id, result);
				break;
			}

			default:
				// console.log("Unrecognized command. ", cmd_id, msg_id);
		}
	}

	private preparePromise<T>(msg_id: number): Promise<T> {
		return new Promise((resolve, reject) => {
			this._promiseResolution.set(msg_id, resolve);
		});
	}

	private resolvePromise(msg_id: number, data: any): void {
		if (this._promiseResolution.has(msg_id)) {
			let func = this._promiseResolution.get(msg_id);
			this._promiseResolution.delete(msg_id);
			func(data);
		}
	}

	private sendEvent(event: string, ... args: any[]): void {
		setTimeout(() => {
			this.emit(event, ...args);
		}, 0);
	}

    private get next_command_id(): number {
        let val = this._next_cmd_id;
        this._next_cmd_id += 1;
        return val;
    }

}


//
// Utility class for parsing system data types out of
// a buffer. Currently, it assumes a little endian byte
// order in the buffer, and that should probably be changed?
//
class DataParser {
	private data: Buffer;
	private view: DataView;
	public offset: number;

    constructor(data: Buffer) {
        this.data = data;
        this.view = new DataView(data.buffer);
        this.offset = 0;
    }

    parseInt32() {
		if (this.offset >= this.data.length) return 0;

        this.offset += 4;
        return this.view.getInt32(this.offset - 4, true);
    }

    parseUint32() {
		if (this.offset >= this.data.length) return 0;

        this.offset += 4;
        return this.view.getUint32(this.offset - 4, true);
    }

    parseString() { 
        let len = this.parseUint32();
        let str = "";
        for (let i=0; i<len; i++) {
			if (this.offset + i >= this.data.length) {
				break;
			}

            str += String.fromCharCode(this.view.getUint8(this.offset + i));
        }
        this.offset += len;

        return str;
    }

	parseBytes(): ArrayBuffer {
        let len      = this.parseUint32();
		let result   = this.data.buffer.slice(this.offset, this.offset + len);
		this.offset += len;

		return result;
	}

    parseBool() {
		if (this.offset >= this.data.length) return false;

        this.offset += 1;
        return this.view.getUint8(this.offset - 1) != 0;
    }
}

