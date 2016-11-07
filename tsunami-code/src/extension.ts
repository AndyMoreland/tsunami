import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import * as tsu from "@derander/tsunami";
import { TsunamiExtension } from "./TsunamiExtension";
import { ImportSymbolCommand } from "./commands/ImportSymbolCommand";
import { ReindexProjectCommand } from "./commands/ReindexProjectCommand";
import { TsunamiCodeActionProvider } from "./plugins/TsunamiCodeActionProvider";
import { TsunamiCodeCompletionProvider } from "./plugins/TsunamiCodeCompletionProvider";

export function activate(context: vscode.ExtensionContext) {
    const projectRoot = vscode.workspace.rootPath;
    console.log("Activating!");

    /* Tsunami is only available in projects. */
    if (!projectRoot) {
        return;
    }

    if (!fs.existsSync(path.join(projectRoot, "tsconfig.json"))) {
        console.log("Aborting tsunami initialization: couldn't find tsconfig at ", path.join(projectRoot, "tsconfig.json"));
        return;
    }

    const settings = JSON.parse(fs.readFileSync(path.join(projectRoot, "tsconfig.json")).toString());

    const tsunami = new tsu.Tsunami(
        new tsu.TsProject(projectRoot, settings)
    );

    const extension = new TsunamiExtension(
        tsunami.getContext(),
        [
            new TsunamiCodeCompletionProvider(tsunami.getContext()),
            new TsunamiCodeActionProvider()
        ],
        [
            new ReindexProjectCommand(tsunami)
        ], [
            new ImportSymbolCommand(tsunami.getContext())
        ]
    );

    extension.bindToContext(context);

    tsunami.buildInitialProjectIndex()
        .then(() => vscode.window.setStatusBarMessage("[tsunami] $(thumbsup) Done indexing: " + path.basename(projectRoot), 3000))
        .catch(e => console.error(e));
}

export function deactivate() {
    /* do nothing */
}
