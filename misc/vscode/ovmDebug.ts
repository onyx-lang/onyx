import {
	Logger, logger,
	LoggingDebugSession,
	InitializedEvent, TerminatedEvent, StoppedEvent, BreakpointEvent, OutputEvent,
	ProgressStartEvent, ProgressUpdateEvent, ProgressEndEvent, InvalidatedEvent,
	Thread, StackFrame, Scope, Source, Handles, Breakpoint, MemoryEvent, ThreadEvent, LoadedSourceEvent, Variable
} from '@vscode/debugadapter';
import { DebugProtocol } from '@vscode/debugprotocol';
import EventEmitter = require('node:events');

import { Subject } from "await-notify";
import * as net from "node:net";
import * as child_process from "node:child_process";
import { ChildProcess } from 'node:child_process';


interface IOVMAttachRequestArguments extends DebugProtocol.AttachRequestArguments {
    socketPath?: string;
    stopOnEntry?: boolean;
}

interface IOVMLaunchRequestArguments extends DebugProtocol.AttachRequestArguments {
    wasmFile: string;
	workingDir: string;
    stopOnEntry?: boolean;
}

interface IFrameReference {
	frameIndex: number;
	threadId: number;
}

export class OVMDebugSession extends LoggingDebugSession {

	private debugger: OVMDebugger;
	private running_process: ChildProcess;

	private _configurationDone: Subject = new Subject();
	private _clientConnectedNotifier: Subject = new Subject();
	private _clientConnected: boolean;

	private _loadedSources: Map<string, Source>;

	private _variableReferences = new Handles<IFrameReference>();
	private _frameReferences = new Handles<IFrameReference>();

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
			this.sendEvent(new StoppedEvent(ev.reason, ev.threadId));
		});

		this.debugger.on("terminated", () => {
			this.sendEvent(new TerminatedEvent());
		});
    }

    protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {
		console.log("INITIALIZE");

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
		response.body.supportsDisassembleRequest = false;
		response.body.supportsSteppingGranularity = true;
		response.body.supportsInstructionBreakpoints = false;

		// make VS Code able to read and write variable memory
		response.body.supportsReadMemoryRequest = false;
		response.body.supportsWriteMemoryRequest = false;

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

		response.body = {
			stackFrames: frames.map((f, i) => {
				let source = new Source(
					this.fileNameToShortName(f.filename),
					this.convertDebuggerPathToClient(f.filename),
					undefined, undefined, "ovm-debug-src"
				);

				if (!this._loadedSources.has(source.name)) {
					this._loadedSources.set(source.name, source);
	
					this.sendEvent(new LoadedSourceEvent("new", source));
				}
				
				let frameRef = this._frameReferences.create({
					threadId: args.threadId,
					frameIndex: i
				});
				return new StackFrame(frameRef, f.funcname, source, f.line);
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
		let varRef   = this._variableReferences.create(frameRef);

		response.body = {
			scopes: [
				new Scope("Locals", varRef, false),
			]
		};
		this.sendResponse(response);	
	}

	protected async variablesRequest(response: DebugProtocol.VariablesResponse, args: DebugProtocol.VariablesArguments, request?: DebugProtocol.Request): Promise<void> {
		const frameRef = this._variableReferences.get(args.variablesReference, {frameIndex: 0, threadId: 0});
		let vs: Variable[] = (await this.debugger.variables(frameRef.frameIndex, frameRef.threadId))
			.map(v => {
				let nv = new Variable(v.name, v.value) as DebugProtocol.Variable;
				nv.type = v.type;
				return nv;
			});

		response.body = { variables: vs };
		this.sendResponse(response);
	}

    protected launchRequest(response: DebugProtocol.LaunchResponse, args: IOVMLaunchRequestArguments, request?: DebugProtocol.Request): void {
		this.running_process = child_process.spawn("onyx-run", ["--debug", args.wasmFile], {
			"cwd": args.workingDir,	
		});

		this.running_process.stdout.setEncoding("utf-8");
		this.running_process.stdout.on("data", (chunk) => {
			this.sendEvent(new OutputEvent(chunk, "console"));
		});

		this.attachRequest(response, {"socketPath": "/tmp/ovm-debug.0000", "stopOnEntry": true});
    }

    protected async attachRequest(response: DebugProtocol.AttachResponse, args: IOVMAttachRequestArguments, request?: DebugProtocol.Request): Promise<void> {
		await this._configurationDone.wait(1000);

		await this.debugger.connect(args.socketPath);

		this._clientConnected = true;
		this._clientConnectedNotifier.notify();

		this.sendResponse(response);
		this.sendEvent(new ThreadEvent("started", 1));

		if (!args.stopOnEntry) {
			this.debugger.resume();
		}
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

	private fileNameToShortName(filename: string): string {
		return filename.substring(filename.lastIndexOf("/") + 1);
	}
}

interface IFileLocation {
	funcname: string;
	filename: string;
	line: number;
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
}

enum OVMCommand {
	NOP     = 0,
	RES     = 1,
	BRK     = 2,
	CLR_BRK = 3,
	STEP    = 5,
	TRACE   = 6,
	THREADS = 7,
	VARS    = 8
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
		});
	}

    resume(thread_id: number = 0xffffffff): void {
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
        let data = new ArrayBuffer(8);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.THREADS, true);

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.THREADS;

		return this.preparePromise(cmd_id);
	}

	variables(frame_index: number, thread_id: number): Promise<IVariableInfo[]> {
        let data = new ArrayBuffer(16);
        let view = new DataView(data);

        let cmd_id = this.next_command_id;

        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.VARS, true);
        view.setUint32(8, frame_index, true);
        view.setUint32(12, thread_id, true);

        this.client.write(new Uint8Array(data));

        this.pending_responses[cmd_id] = OVMCommand.VARS;

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
					}

					this.sendEvent("paused", { reason, threadId: thread_id });
					break;
				}

				case OVMEvent.RESPONSE: {
					this.handleResponse(parser);
					break;
				}

				default:
					console.log("Unknown event: ", event_id, data);
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

					result.push({funcname, filename, line});
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
					result.push({name, value, type});
				}

				this.resolvePromise(msg_id, result);
				break;
			}

			default:
				console.log("Unrecognized command. ", cmd_id, msg_id);
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
            str += String.fromCharCode(this.view.getUint8(this.offset + i));
        }
        this.offset += len;

        return str;
    }

    parseBool() {
		if (this.offset >= this.data.length) return 0;

        this.offset += 1;
        return this.view.getUint8(this.offset - 1) != 0;
    }
}

