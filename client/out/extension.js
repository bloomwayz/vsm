"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const vscode_1 = require("vscode");
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
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
    const serverOptions = {
        run: { command, transport: node_1.TransportKind.stdio },
        debug: { command, transport: node_1.TransportKind.stdio }
    };
    // Options to control the language client
    const clientOptions = {
        // Register the server for M documents (*.m)
        documentSelector: [{ scheme: 'file', language: 'mlang' }],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/.clientrc')
        }
    };
    // Create the language client and start the client.
    client = new node_1.LanguageClient('mlsp', 'M Language Server', serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map