// Part of this code was taken from the example vscode debugger found here,
// https://github.com/microsoft/vscode-mock-debug/blob/5524e7a3dd9ec176a987ffe3aebd489f1f543799/src/debugAdapter.ts`

import { OVMDebugSession } from './ovmDebug';

import * as Net from 'node:net';
/*
 * When the debug adapter is run as an external process,
 * normally the helper function DebugSession.run(...) takes care of everything:
 *
 * 	MockDebugSession.run(MockDebugSession);
 *
 * but here the helper is not flexible enough to deal with a debug session constructors with a parameter.
 * So for now we copied and modified the helper:
 */

// first parse command line arguments to see whether the debug adapter should run as a server
let port = 0;
const args = process.argv.slice(2);
args.forEach(function (val, index, array) {
	const portMatch = /^--server=(\d{4,5})$/.exec(val);
	if (portMatch) {
		port = parseInt(portMatch[1], 10);
	}
});

if (port > 0) {
	Net.createServer((socket) => {
		const session = new OVMDebugSession();
		session.setRunAsServer(true);
		session.start(socket, socket);
	}).listen(port);

} else {

	// start a single session that communicates via stdin/stdout
	const session = new OVMDebugSession();
	process.on('SIGTERM', () => {
		session.shutdown();
	});
	session.start(process.stdin, process.stdout);
}