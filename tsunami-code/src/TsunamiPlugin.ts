import * as vs from "vscode";

export interface TsunamiPlugin {
    bindToContext(context: vs.ExtensionContext): void;
}
