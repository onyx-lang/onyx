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
const vslc = require("vscode-languageclient/node");
let client;
function activate(context) {
    return __awaiter(this, void 0, void 0, function* () {
        let console = vscode.window.createOutputChannel("Onyx Extension");
        console.appendLine("Starting Onyx Extension");
        const selector = { language: 'onyx', scheme: "file" };
        const engine = new vsctmls.textmateEngine.TextmateEngine('onyx', 'source.onyx');
        const documentSymbolProvider = new vsctmls.documentSymbols.DocumentSymbolProvider(engine);
        const workspaceSymbolProvider = new vsctmls.workspaceSymbols.WorkspaceSymbolProvider('onyx', documentSymbolProvider);
        const peekFileDefinitionProvider = new vsctmls.peekDefinitions.PeekDefinitionProvider(documentSymbolProvider);
        context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(selector, documentSymbolProvider));
        context.subscriptions.push(vscode.languages.registerWorkspaceSymbolProvider(workspaceSymbolProvider));
        context.subscriptions.push(vscode.languages.registerDefinitionProvider({ 'language': 'onyx' }, peekFileDefinitionProvider));
        let onyx_path = process.env['ONYX_PATH'];
        if (!onyx_path) {
            onyx_path = require('os').homedir() + "/.onyx";
            process.env["ONYX_PATH"] = onyx_path;
            process.env["PATH"] = process.env["PATH"] + ":" + onyx_path + "/bin";
        }
        let executable = `${onyx_path}/bin/onyx`;
        if (process.platform == "win32") {
            // Windows distributions are different
            executable = `${onyx_path}/onyx`;
        }
        if (onyx_path) {
            let serverOptions = {
                command: executable,
                args: ["lsp"],
                transport: vslc.TransportKind.stdio,
            };
            let clientOptions = {
                documentSelector: [
                    { scheme: "file", language: "onyx" },
                ],
                connectionOptions: {
                    cancellationStrategy: null,
                    maxRestartCount: 5
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
