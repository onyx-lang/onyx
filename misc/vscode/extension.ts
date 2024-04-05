import * as vscode from "vscode";
import * as vsctmls from 'vscode-textmate-languageservice';
import * as fs from "fs";

import * as vslc2 from "vscode-languageclient";
import * as vslc from "vscode-languageclient/node";
import * as cp from "child_process";

let client: vslc.LanguageClient;

function get_onyx_path() {
	let onyx_path = process.env['ONYX_PATH'];
	if (!onyx_path) {
		onyx_path = require('os').homedir() + "/.onyx";
		process.env["ONYX_PATH"] = onyx_path;
		process.env["PATH"] = process.env["PATH"] + ":" + onyx_path + "/bin"
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
				}))

			if (sessionEntries.length == 0) { rej(new Error("No sessions waiting to be debugged")); return; }
			if (sessionEntries.length == 1) { res(`${onyx_path}/debug.${sessionEntries[0].pid}`); return; }

			const quickPick = vscode.window.createQuickPick<{label: string, pid: number}>();
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

export async function activate(context: vscode.ExtensionContext) {
	let console = vscode.window.createOutputChannel("Onyx Extension");
	console.appendLine("Starting Onyx Extension");

	registerCommands();

	const selector: vscode.DocumentSelector = { language: 'onyx', scheme: "file" };
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
		let serverOptions = async () => {
			return cp.spawn(executable, ["lsp"], {
				detached: true,
				cwd: vscode.workspace.workspaceFolders[0].uri.fsPath
			});
		}

		let clientOptions: vslc.LanguageClientOptions = {
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
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}

	return client.stop();	
}
