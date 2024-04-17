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
exports.deactivate = exports.activate = void 0;
const vscode = require("vscode");
const vsctmls = require("vscode-textmate-languageservice");
const fs = require("fs");
const vslc = require("vscode-languageclient/node");
const cp = require("child_process");
let client;
function get_onyx_path() {
    let onyx_path = process.env['ONYX_PATH'];
    if (!onyx_path) {
        onyx_path = require('os').homedir() + "/.onyx";
        process.env["ONYX_PATH"] = onyx_path;
        process.env["PATH"] = process.env["PATH"] + ":" + onyx_path + "/bin";
    }
    return onyx_path;
}
function registerCommands() {
    vscode.commands.registerCommand('extension.pickOnyxSession', () => {
        return new Promise((res, rej) => {
            let onyx_path = get_onyx_path();
            const sessionEntries = fs
                .readdirSync(onyx_path)
                .filter(file => file.startsWith("debug."))
                .map(file => ({
                label: file.split(".")[1],
                pid: parseInt(file.split(".")[1])
            }));
            if (sessionEntries.length == 0) {
                rej(new Error("No sessions waiting to be debugged"));
                return;
            }
            if (sessionEntries.length == 1) {
                res(`${onyx_path}/debug.${sessionEntries[0].pid}`);
                return;
            }
            const quickPick = vscode.window.createQuickPick();
            quickPick.title = "Attach to session";
            quickPick.canSelectMany = false;
            quickPick.matchOnDescription = true;
            quickPick.matchOnDetail = true;
            quickPick.placeholder = "Select the session to attach to";
            quickPick.items = sessionEntries;
            quickPick.onDidAccept(() => {
                if (quickPick.selectedItems.length !== 1) {
                    rej(new Error(`Session not selected. case one`));
                }
                const selectedId = quickPick.selectedItems[0].pid;
                quickPick.dispose();
                res(`${onyx_path}/debug.${selectedId}`);
            });
            quickPick.onDidHide(() => {
                quickPick.dispose();
            });
            quickPick.show();
        });
    });
}
function activate(context) {
    return __awaiter(this, void 0, void 0, function* () {
        let console = vscode.window.createOutputChannel("Onyx Extension");
        console.appendLine("Starting Onyx Extension");
        registerCommands();
        const selector = { language: 'onyx', scheme: "file" };
        const engine = new vsctmls.textmateEngine.TextmateEngine('onyx', 'source.onyx');
        const documentSymbolProvider = new vsctmls.documentSymbols.DocumentSymbolProvider(engine);
        const workspaceSymbolProvider = new vsctmls.workspaceSymbols.WorkspaceSymbolProvider('onyx', documentSymbolProvider);
        const peekFileDefinitionProvider = new vsctmls.peekDefinitions.PeekDefinitionProvider(documentSymbolProvider);
        context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(selector, documentSymbolProvider));
        context.subscriptions.push(vscode.languages.registerWorkspaceSymbolProvider(workspaceSymbolProvider));
        context.subscriptions.push(vscode.languages.registerDefinitionProvider({ 'language': 'onyx' }, peekFileDefinitionProvider));
        let onyx_path = get_onyx_path();
        let executable = `${onyx_path}/bin/onyx`;
        if (process.platform == "win32") {
            // Windows distributions are different
            executable = `${onyx_path}/onyx`;
        }
        console.appendLine(`Onyx executable is: ${executable}`);
        if (onyx_path) {
            let serverOptions = () => __awaiter(this, void 0, void 0, function* () {
                return cp.spawn(executable, ["lsp"], {
                    detached: true,
                    cwd: vscode.workspace.workspaceFolders[0].uri.fsPath
                });
            });
            let clientOptions = {
                documentSelector: [
                    { scheme: "file", language: "onyx" },
                ],
                connectionOptions: {
                    cancellationStrategy: null,
                    maxRestartCount: 5
                },
                uriConverters: {
                    code2Protocol: (x) => x.fsPath,
                    protocol2Code: (x) => vscode.Uri.parse(decodeURIComponent(x))
                }
            };
            client = new vslc.LanguageClient("onyx-lsp", serverOptions, clientOptions);
            client.start();
        }
        console.appendLine("Onyx Extension loaded.");
    });
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
