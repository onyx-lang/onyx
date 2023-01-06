import * as vscode from "vscode";
import * as vsctmls from 'vscode-textmate-languageservice';

import * as vslc2 from "vscode-languageclient";
import * as vslc from "vscode-languageclient/node";
import { TransportKind } from "vscode-languageclient/node";

let client: vslc.LanguageClient;

export async function activate(context: vscode.ExtensionContext) {
	let console = vscode.window.createOutputChannel("Onyx Extension");
	console.appendLine("Starting Onyx Extension");

	const selector: vscode.DocumentSelector = { language: 'onyx', scheme: "file" };
	const engine = new vsctmls.textmateEngine.TextmateEngine('onyx', 'source.onyx');
	const documentSymbolProvider = new vsctmls.documentSymbols.DocumentSymbolProvider(engine);
	const workspaceSymbolProvider = new vsctmls.workspaceSymbols.WorkspaceSymbolProvider('onyx', documentSymbolProvider);
	// const foldingProvider = new vsctmls.folding.FoldingProvider(engine);
	const peekFileDefinitionProvider = new vsctmls.peekDefinitions.PeekDefinitionProvider(documentSymbolProvider);

	context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(selector, documentSymbolProvider));
	context.subscriptions.push(vscode.languages.registerWorkspaceSymbolProvider(workspaceSymbolProvider));
	// context.subscriptions.push(vscode.languages.registerFoldingRangeProvider(selector, foldingProvider));
	context.subscriptions.push(vscode.languages.registerDefinitionProvider({ 'language': 'onyx' }, peekFileDefinitionProvider));

	// client = new vslc.LanguageClient("onyx-lsp", {
	// 	command: "onyx-lsp",
	// 	transport: TransportKind.stdio,
	// }, {
	// 	documentSelector: [
	// 		{ scheme: "file", language: "onyx" },
	// 	],
	// 	synchronize: {
	// 		fileEvents: vscode.workspace.createFileSystemWatcher("**/*.onyx")
	// 	}
	// });

	// client.start();

	console.appendLine("Onyx Extension loaded.");
}
