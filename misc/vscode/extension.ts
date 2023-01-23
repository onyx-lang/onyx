import * as vscode from "vscode";
import * as vsctmls from 'vscode-textmate-languageservice';

import * as vslc2 from "vscode-languageclient";
import * as vslc from "vscode-languageclient/node";

let client: vslc.LanguageClient;

export async function activate(context: vscode.ExtensionContext) {
	let console = vscode.window.createOutputChannel("Onyx Extension");
	console.appendLine("Starting Onyx Extension");

	const selector: vscode.DocumentSelector = { language: 'onyx', scheme: "file" };
	const engine = new vsctmls.textmateEngine.TextmateEngine('onyx', 'source.onyx');
	const documentSymbolProvider = new vsctmls.documentSymbols.DocumentSymbolProvider(engine);
	const workspaceSymbolProvider = new vsctmls.workspaceSymbols.WorkspaceSymbolProvider('onyx', documentSymbolProvider);
	const peekFileDefinitionProvider = new vsctmls.peekDefinitions.PeekDefinitionProvider(documentSymbolProvider);

	context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(selector, documentSymbolProvider));
	context.subscriptions.push(vscode.languages.registerWorkspaceSymbolProvider(workspaceSymbolProvider));
	context.subscriptions.push(vscode.languages.registerDefinitionProvider({ 'language': 'onyx' }, peekFileDefinitionProvider));

	let serverOptions: vslc.ServerOptions = {
		command: "onyx-lsp",
		transport: vslc.TransportKind.stdio,
	};

	let clientOptions: vslc.LanguageClientOptions = {
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

	console.appendLine("Onyx Extension loaded.");
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}

	return client.stop();	
}
