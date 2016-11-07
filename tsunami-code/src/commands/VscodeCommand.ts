import * as vs from "vscode";

export interface VscodeCommand {
    commandName: string;
    execute(...args: any[]): Promise<void>;
}

export interface VscodeTextEditorCommand {
    commandName: string;
    execute(editor: vs.TextEditor, edit: vs.TextEditorEdit, ...args: any[]): Promise<void>;
}
