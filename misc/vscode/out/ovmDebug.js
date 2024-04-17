"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.OVMDebugSession = void 0;
const debugadapter_1 = require("@vscode/debugadapter");
const EventEmitter = require("node:events");
const await_notify_1 = require("await-notify");
const net = require("node:net");
const child_process = require("node:child_process");
const fs = require("node:fs");
const logger_1 = require("@vscode/debugadapter/lib/logger");
class OVMDebugSession extends debugadapter_1.LoggingDebugSession {
    constructor() {
        super("ovm-debug-log.txt");
        this._configurationDone = new await_notify_1.Subject();
        this._clientConnectedNotifier = new await_notify_1.Subject();
        this._variableReferences = new debugadapter_1.Handles();
        this._frameReferences = new debugadapter_1.Handles();
        this.setDebuggerLinesStartAt1(true);
        this.setDebuggerColumnsStartAt1(true);
        this._loadedSources = new Map();
        this._clientConnected = false;
        this.debugger = new OVMDebugger();
        this.debugger.on("breakpointHit", (ev) => {
            this.sendEvent(new debugadapter_1.StoppedEvent("breakpoint", ev.threadId));
        });
        this.debugger.on("paused", (ev) => {
            if (ev.reason == "entry") {
                this.sendEvent(new debugadapter_1.ThreadEvent("started", ev.thread_id));
                if (!this.stopOnEntry) {
                    this.debugger.resume(ev.thread_id);
                    return;
                }
            }
            this.sendEvent(new debugadapter_1.StoppedEvent(ev.reason, ev.threadId));
        });
        this.debugger.on("terminated", () => {
            this.sendEvent(new debugadapter_1.TerminatedEvent());
        });
    }
    initializeRequest(response, args) {
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
        this.sendEvent(new debugadapter_1.InitializedEvent());
    }
    configurationDoneRequest(response, args, request) {
        super.configurationDoneRequest(response, args);
        this._configurationDone.notify();
    }
    disconnectRequest(response, args, request) {
        if (args.terminateDebuggee) {
            if (this.running_process) {
                this.running_process.kill('SIGTERM');
            }
        }
        this.sendResponse(response);
    }
    cancelRequest(response, args, request) {
        this.sendResponse(response);
    }
    setBreakPointsRequest(response, args, request) {
        return __awaiter(this, void 0, void 0, function* () {
            while (!this._clientConnected) {
                yield this._clientConnectedNotifier.wait();
            }
            const path = args.source.path;
            const clientLines = args.lines || [];
            yield this.debugger.remove_breakpoints_in_file(path);
            const actualBreakpointsPromise = clientLines.map((line) => __awaiter(this, void 0, void 0, function* () {
                const res = yield this.debugger.set_breakpoint(path, line);
                const bp = new debugadapter_1.Breakpoint(res.verified, this.convertDebuggerLineToClient(res.line));
                bp.id = res.id;
                return bp;
            }));
            const actualBreakpoints = yield Promise.all(actualBreakpointsPromise);
            response.body = {
                breakpoints: actualBreakpoints
            };
            this.sendResponse(response);
        });
    }
    stackTraceRequest(response, args, request) {
        return __awaiter(this, void 0, void 0, function* () {
            let frames = yield this.debugger.trace(args.threadId);
            this._frameReferences.reset();
            this._variableReferences.reset();
            response.body = {
                stackFrames: frames.map((f, i) => {
                    let source = this.loadSource(f.filename);
                    let frameRef = this._frameReferences.create({
                        threadId: args.threadId,
                        frameIndex: i
                    });
                    let stack_frame = new debugadapter_1.StackFrame(frameRef, f.funcname, source, f.line);
                    stack_frame.instructionPointerReference = f.instructionPointer.toString();
                    return stack_frame;
                })
            };
            this.sendResponse(response);
        });
    }
    threadsRequest(response, request) {
        return __awaiter(this, void 0, void 0, function* () {
            let threads = yield this.debugger.threads();
            response.body = {
                threads: threads.map(t => new debugadapter_1.Thread(t.id, t.name))
            };
            this.sendResponse(response);
        });
    }
    scopesRequest(response, args, request) {
        let frameId = args.frameId;
        let frameRef = this._frameReferences.get(frameId);
        let varRef = this._variableReferences.create(Object.assign(Object.assign({}, frameRef), { variableChain: [] }));
        response.body = {
            scopes: [
                new debugadapter_1.Scope("Locals", varRef, false),
            ]
        };
        this.sendResponse(response);
    }
    variablesRequest(response, args, request) {
        return __awaiter(this, void 0, void 0, function* () {
            const frameRef = this._variableReferences.get(args.variablesReference, { frameIndex: 0, threadId: 0, variableChain: [] });
            let vs = (yield this.debugger.variables(frameRef.frameIndex, frameRef.threadId, frameRef.variableChain))
                .map(v => {
                let nv = new debugadapter_1.Variable(v.name, v.value);
                nv.type = v.type;
                if (v.hasChildren) {
                    nv.variablesReference = this._variableReferences.create(Object.assign(Object.assign({}, frameRef), { variableChain: frameRef.variableChain.concat([v.symId]) }));
                }
                if (v.memoryReference > 0) {
                    nv.memoryReference = v.memoryReference.toString();
                }
                return nv;
            });
            response.body = { variables: vs };
            this.sendResponse(response);
        });
    }
    launchRequest(response, args, request) {
        return __awaiter(this, void 0, void 0, function* () {
            let debugSocketPath = "/tmp/onyx-debug-socket";
            if (fs.existsSync(debugSocketPath)) {
                fs.unlinkSync(debugSocketPath);
            }
            this.sendEvent(new logger_1.LogOutputEvent(`Spawning Onyx debug session\nSocket: ${debugSocketPath}\nWorking Dir: ${args.workingDir}\n`, logger_1.LogLevel.Log));
            let onyx_path = `${args.onyxPath}/bin/onyx`;
            if (args.wasmFile) {
                this.running_process = child_process.spawn(onyx_path, ["run", "--debug", "--debug-socket", debugSocketPath, args.wasmFile], {
                    "cwd": args.workingDir,
                    "env": {
                        "ONYX_PATH": args.onyxPath,
                    }
                });
            }
            else if (args.onyxFiles) {
                this.running_process = child_process.spawn(onyx_path, ["run", "--debug", "--debug-socket", debugSocketPath, ...args.onyxFiles], {
                    "cwd": args.workingDir,
                    "env": {
                        "ONYX_PATH": args.onyxPath,
                    }
                });
            }
            else {
                this.sendErrorResponse(response, {
                    format: "Expected either wasmFile or onyxFiles in launch configuration.",
                    id: 1
                });
                return;
            }
            if (!this.running_process.pid) {
                this.sendErrorResponse(response, {
                    format: "Failed to spawn Onyx debug session.",
                    id: 1
                });
                return;
            }
            this.running_process.stdout.setEncoding("utf-8");
            this.running_process.stdout.on("data", (chunk) => {
                this.sendEvent(new debugadapter_1.OutputEvent(chunk, "console"));
            });
            this.sendEvent(new logger_1.LogOutputEvent(`Process is spawned: ${this.running_process.pid}\n`, logger_1.LogLevel.Log));
            let done_hack = false;
            let success = Promise.race([
                new Promise((res, rej) => setTimeout(() => res(false), 2000)),
                new Promise((res, rej) => __awaiter(this, void 0, void 0, function* () {
                    while (!done_hack) {
                        if (fs.existsSync(debugSocketPath)) {
                            res(true);
                        }
                        yield new Promise((res, rej) => setTimeout(res, 100));
                    }
                }))
            ]);
            done_hack = true;
            if (!success) {
                this.sendErrorResponse(response, {
                    format: "Failed to spawn Onyx debug session.",
                    id: 1
                });
                return;
            }
            // This "sleep" is very hacky and needs to be replaced. The problem
            // is the we need to wait until the socket exists.
            yield this.attachRequest(response, { "socketPath": debugSocketPath, "stopOnEntry": args.stopOnEntry });
        });
    }
    attachRequest(response, args, request) {
        var _a;
        return __awaiter(this, void 0, void 0, function* () {
            this.sendEvent(new logger_1.LogOutputEvent(`Connecting to process\n`, logger_1.LogLevel.Log));
            yield this._configurationDone.wait(1000);
            try {
                yield this.debugger.connect(args.socketPath);
            }
            catch (e) {
                this.sendEvent(new logger_1.LogOutputEvent(`Error connecting to session: ${e.toString()}`, logger_1.LogLevel.Error));
                this.sendErrorResponse(response, 41);
                return;
            }
            this.sendEvent(new logger_1.LogOutputEvent(`Connected to process`, logger_1.LogLevel.Log));
            this._clientConnected = true;
            this._clientConnectedNotifier.notify();
            this.stopOnEntry = (_a = args.stopOnEntry) !== null && _a !== void 0 ? _a : false;
            this.sendResponse(response);
        });
    }
    pauseRequest(response, args, request) {
        this.debugger.pause(args.threadId);
        this.sendResponse(response);
    }
    continueRequest(response, args, request) {
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
    nextRequest(response, args, request) {
        this.debugger.step("over", args.threadId);
        this.sendResponse(response);
    }
    stepOutRequest(response, args, request) {
        this.debugger.step("out", args.threadId);
        this.sendResponse(response);
    }
    stepInRequest(response, args, request) {
        this.debugger.step("line", args.threadId);
        this.sendResponse(response);
    }
    disassembleRequest(response, args, request) {
        return __awaiter(this, void 0, void 0, function* () {
            let addr = parseInt(args.memoryReference) + args.offset;
            let instrs = yield this.debugger.disassemble(addr, args.instructionCount);
            response.body = {
                instructions: instrs.map((i, index) => {
                    let source;
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
        });
    }
    readMemoryRequest(response, args, request) {
        return __awaiter(this, void 0, void 0, function* () {
            let addr = parseInt(args.memoryReference) + args.offset;
            let mem = yield this.debugger.read_memory(addr, args.count);
            response.body = {
                address: addr.toString(),
                data: Buffer.from(mem.data).toString("base64"),
            };
            this.sendResponse(response);
        });
    }
    writeMemoryRequest(response, args, request) {
        return __awaiter(this, void 0, void 0, function* () {
            let addr = parseInt(args.memoryReference) + args.offset;
            let data = Buffer.from(args.data, "base64");
            let bytesWritten = yield this.debugger.write_memory(addr, data);
            response.body = {
                bytesWritten: bytesWritten,
            };
            this.sendResponse(response);
        });
    }
    fileNameToShortName(filename) {
        return filename.substring(filename.lastIndexOf("/") + 1);
    }
    loadSource(filename) {
        let source = new debugadapter_1.Source(this.fileNameToShortName(filename), this.convertDebuggerPathToClient(filename), undefined, undefined, "ovm-debug-src");
        if (!this._loadedSources.has(source.name)) {
            this._loadedSources.set(source.name, source);
            this.sendEvent(new debugadapter_1.LoadedSourceEvent("new", source));
        }
        return source;
    }
}
exports.OVMDebugSession = OVMDebugSession;
var OVMCommand;
(function (OVMCommand) {
    OVMCommand[OVMCommand["NOP"] = 0] = "NOP";
    OVMCommand[OVMCommand["RES"] = 1] = "RES";
    OVMCommand[OVMCommand["PAUSE"] = 2] = "PAUSE";
    OVMCommand[OVMCommand["BRK"] = 3] = "BRK";
    OVMCommand[OVMCommand["CLR_BRK"] = 4] = "CLR_BRK";
    OVMCommand[OVMCommand["STEP"] = 5] = "STEP";
    OVMCommand[OVMCommand["TRACE"] = 6] = "TRACE";
    OVMCommand[OVMCommand["THREADS"] = 7] = "THREADS";
    OVMCommand[OVMCommand["VARS"] = 8] = "VARS";
    OVMCommand[OVMCommand["MEM_R"] = 9] = "MEM_R";
    OVMCommand[OVMCommand["MEM_W"] = 10] = "MEM_W";
    OVMCommand[OVMCommand["DISASM"] = 11] = "DISASM";
})(OVMCommand || (OVMCommand = {}));
var OVMEvent;
(function (OVMEvent) {
    OVMEvent[OVMEvent["NOP"] = 0] = "NOP";
    OVMEvent[OVMEvent["BREAKPOINT_HIT"] = 1] = "BREAKPOINT_HIT";
    OVMEvent[OVMEvent["PAUSED"] = 2] = "PAUSED";
    OVMEvent[OVMEvent["RESPONSE"] = 4294967295] = "RESPONSE";
})(OVMEvent || (OVMEvent = {}));
class OVMDebugger extends EventEmitter {
    constructor() {
        super();
        this._promiseResolution = new Map();
    }
    connect(path) {
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
    pause(thread_id = 0xffffffff) {
        if (this.client == null)
            return;
        let data = new ArrayBuffer(12);
        let view = new DataView(data);
        let cmd_id = this.next_command_id;
        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.PAUSE, true);
        view.setUint32(8, thread_id, true);
        this.client.write(new Uint8Array(data));
        this.pending_responses[cmd_id] = OVMCommand.PAUSE;
    }
    resume(thread_id = 0xffffffff) {
        if (this.client == null)
            return;
        let data = new ArrayBuffer(12);
        let view = new DataView(data);
        let cmd_id = this.next_command_id;
        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.RES, true);
        view.setUint32(8, thread_id, true);
        this.client.write(new Uint8Array(data));
        this.pending_responses[cmd_id] = OVMCommand.RES;
    }
    set_breakpoint(filename, line) {
        return __awaiter(this, void 0, void 0, function* () {
            if (this.client == null)
                return Promise.resolve({});
            let data = new ArrayBuffer(16 + filename.length);
            let view = new DataView(data);
            let cmd_id = this.next_command_id;
            view.setUint32(0, cmd_id, true);
            view.setUint32(4, OVMCommand.BRK, true);
            view.setUint32(8, filename.length, true);
            for (let i = 0; i < filename.length; i++) {
                view.setUint8(i + 12, filename.charCodeAt(i));
            }
            view.setUint32(12 + filename.length, line, true);
            this.client.write(new Uint8Array(data));
            this.pending_responses[cmd_id] = OVMCommand.BRK;
            return this.preparePromise(cmd_id);
        });
    }
    remove_breakpoints_in_file(filename) {
        return __awaiter(this, void 0, void 0, function* () {
            if (this.client == null)
                return Promise.resolve(false);
            let data = new ArrayBuffer(12 + filename.length);
            let view = new DataView(data);
            let cmd_id = this.next_command_id;
            view.setUint32(0, cmd_id, true);
            view.setUint32(4, OVMCommand.CLR_BRK, true);
            view.setUint32(8, filename.length, true);
            for (let i = 0; i < filename.length; i++) {
                view.setUint8(i + 12, filename.charCodeAt(i));
            }
            this.client.write(new Uint8Array(data));
            this.pending_responses[cmd_id] = OVMCommand.CLR_BRK;
            return this.preparePromise(cmd_id);
        });
    }
    step(granularity, thread_id) {
        if (this.client == null)
            return;
        let data = new ArrayBuffer(16);
        let view = new DataView(data);
        let cmd_id = this.next_command_id;
        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.STEP, true);
        view.setUint32(12, thread_id, true);
        switch (granularity) {
            case "line":
                view.setUint32(8, 1, true);
                break;
            case "instruction":
                view.setUint32(8, 2, true);
                break;
            case "over":
                view.setUint32(8, 3, true);
                break;
            case "out":
                view.setUint32(8, 4, true);
                break;
        }
        this.client.write(new Uint8Array(data));
        this.pending_responses[cmd_id] = OVMCommand.STEP;
    }
    trace(thread_id) {
        if (this.client == null)
            return Promise.resolve([]);
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
    threads() {
        if (this.client == null)
            return Promise.resolve([]);
        let data = new ArrayBuffer(8);
        let view = new DataView(data);
        let cmd_id = this.next_command_id;
        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.THREADS, true);
        this.client.write(new Uint8Array(data));
        this.pending_responses[cmd_id] = OVMCommand.THREADS;
        return this.preparePromise(cmd_id);
    }
    variables(frame_index, thread_id, descention) {
        if (this.client == null)
            return Promise.resolve([]);
        let data = new ArrayBuffer(20 + 4 * descention.length);
        let view = new DataView(data);
        let cmd_id = this.next_command_id;
        view.setUint32(0, cmd_id, true);
        view.setUint32(4, OVMCommand.VARS, true);
        view.setUint32(8, frame_index, true);
        view.setUint32(12, thread_id, true);
        view.setUint32(16, descention.length, true);
        for (let i = 0; i < descention.length; i++) {
            view.setUint32(20 + i * 4, descention[i], true);
        }
        this.client.write(new Uint8Array(data));
        this.pending_responses[cmd_id] = OVMCommand.VARS;
        return this.preparePromise(cmd_id);
    }
    read_memory(addr, count) {
        if (this.client == null)
            return Promise.resolve({});
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
    write_memory(addr, data_to_write) {
        if (this.client == null)
            return Promise.resolve(0);
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
    disassemble(addr, count) {
        if (this.client == null)
            return Promise.resolve([]);
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
    parseIncoming(data) {
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
                        case 1:
                            reason = "entry";
                            break;
                        case 2:
                            reason = "step";
                            break;
                        case 3:
                            reason = "exception";
                            break;
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
    handleResponse(parser) {
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
                let bp_id = parser.parseInt32();
                let line = parser.parseInt32();
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
                let result = new Array();
                let count = parser.parseUint32();
                for (let i = 0; i < count; i++) {
                    let funcname = parser.parseString();
                    let filename = parser.parseString();
                    let line = parser.parseInt32();
                    let ip = parser.parseInt32();
                    result.push({ funcname, filename, line, instructionPointer: ip });
                }
                this.resolvePromise(msg_id, result);
                break;
            }
            case OVMCommand.THREADS: {
                let result = new Array();
                let count = parser.parseUint32();
                for (let i = 0; i < count; i++) {
                    let id = parser.parseUint32();
                    let name = parser.parseString();
                    result.push({ id, name });
                }
                this.resolvePromise(msg_id, result);
                break;
            }
            case OVMCommand.VARS: {
                let result = new Array();
                while (parser.parseInt32() == 0) {
                    let name = parser.parseString();
                    let value = parser.parseString();
                    let type = parser.parseString();
                    let symId = parser.parseUint32();
                    let hasChildren = parser.parseBool();
                    let memoryReference = parser.parseUint32();
                    result.push({ name, value, type, symId, hasChildren, memoryReference });
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
                let result = new Array();
                while (parser.parseInt32() == 0) {
                    let instr = parser.parseString();
                    let line = parser.parseUint32();
                    let newSource = null;
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
    preparePromise(msg_id) {
        return new Promise((resolve, reject) => {
            this._promiseResolution.set(msg_id, resolve);
        });
    }
    resolvePromise(msg_id, data) {
        if (this._promiseResolution.has(msg_id)) {
            let func = this._promiseResolution.get(msg_id);
            this._promiseResolution.delete(msg_id);
            func(data);
        }
    }
    sendEvent(event, ...args) {
        setTimeout(() => {
            this.emit(event, ...args);
        }, 0);
    }
    get next_command_id() {
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
    constructor(data) {
        this.data = data;
        this.view = new DataView(data.buffer);
        this.offset = 0;
    }
    parseInt32() {
        if (this.offset >= this.data.length)
            return 0;
        this.offset += 4;
        return this.view.getInt32(this.offset - 4, true);
    }
    parseUint32() {
        if (this.offset >= this.data.length)
            return 0;
        this.offset += 4;
        return this.view.getUint32(this.offset - 4, true);
    }
    parseString() {
        let len = this.parseUint32();
        let str = "";
        for (let i = 0; i < len; i++) {
            if (this.offset + i >= this.data.length) {
                break;
            }
            str += String.fromCharCode(this.view.getUint8(this.offset + i));
        }
        this.offset += len;
        return str;
    }
    parseBytes() {
        let len = this.parseUint32();
        let result = this.data.buffer.slice(this.offset, this.offset + len);
        this.offset += len;
        return result;
    }
    parseBool() {
        if (this.offset >= this.data.length)
            return false;
        this.offset += 1;
        return this.view.getUint8(this.offset - 1) != 0;
    }
}
