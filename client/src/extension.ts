import * as path from 'path';
import { workspace, ExtensionContext, commands } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	// commands.executeCommand('setContext', 'inCoqProject', true);
	const command = '/Users/young/Desktop/vsm/language_server/_build/default/bin/main.exe';
	// const command = path.join('_build', 'default', 'language_server', 'bin', 'main.exe');
	// context.asAbsolutePath(
	// 	path.join('_build', 'default', 'language_server', 'bin', 'main.exe')
	// );

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used

	// const ServerOptions = Executable | {
	// 	run: Executable;
	// 	debug: Executable;
	// }
	const serverOptions: ServerOptions = {
		run: { command, transport: TransportKind.stdio },
		debug: { command, transport: TransportKind.stdio }
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for M documents (*.m)
		documentSelector: [{ scheme: 'file', language: 'mlang'}],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
		
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'mlsp',
		'M Language Server',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}