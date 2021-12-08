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
const vscode = require("vscode");
const vsctmls = require("vscode-textmate-languageservice");
function activate(context) {
    return __awaiter(this, void 0, void 0, function* () {
        let console = vscode.window.createOutputChannel("Onyx Extension");
        console.appendLine("Starting Onyx Extension");
        const selector = { language: 'onyx', scheme: "file" };
        const engine = new vsctmls.textmateEngine.TextmateEngine('onyx', 'source.onyx');
        const documentSymbolProvider = new vsctmls.documentSymbols.DocumentSymbolProvider(engine);
        const workspaceSymbolProvider = new vsctmls.workspaceSymbols.WorkspaceSymbolProvider('onyx', documentSymbolProvider);
        // const foldingProvider = new vsctmls.folding.FoldingProvider(engine);
        const peekFileDefinitionProvider = new vsctmls.peekDefinitions.PeekDefinitionProvider(documentSymbolProvider);
        context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(selector, documentSymbolProvider));
        context.subscriptions.push(vscode.languages.registerWorkspaceSymbolProvider(workspaceSymbolProvider));
        // context.subscriptions.push(vscode.languages.registerFoldingRangeProvider(selector, foldingProvider));
        context.subscriptions.push(vscode.languages.registerDefinitionProvider({ 'language': 'onyx' }, peekFileDefinitionProvider));
        console.appendLine("Onyx Extension loaded.");
    });
}
exports.activate = activate;
