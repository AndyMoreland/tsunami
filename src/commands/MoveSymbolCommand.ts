import * as Bluebird from "bluebird";
import { ImportEditor } from "../imports/ImportEditor";
import { AbsoluteFilename, ModuleSpecifier } from "../imports/ImportStatement";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { CodeEditGroup } from "../protocol/types";
import { rewriteSymbolImportInFile } from "../utilities/moveSymbolUtils";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";

export interface MoveSymbolCommand extends Command {
    arguments: {
        fromFilename: string;
        toFilename: string;
        symbolName: string;
    };
}

function assertAbsolute(filename: string): AbsoluteFilename {
    if (!filename.startsWith("/") && !filename.startsWith("\\")) {
        throw new Error("Must pass absolute filename.");
    }

    return filename as AbsoluteFilename;
}

export class MoveSymbolCommandDefinition implements CommandDefinition<MoveSymbolCommand, CodeEditGroup[] | null> {
    public predicate(command: Command): command is MoveSymbolCommand {
        return command.command === "MOVE_SYMBOL";
    }

    public async processor(context: TsunamiContext, command: MoveSymbolCommand): Bluebird<CodeEditGroup[] | null> {
        const { fromFilename, toFilename, symbolName } = command.arguments;

        if (fromFilename === toFilename) {
            return Bluebird.reject(new Error("Can't move symbol from file to itself."));
        }

        const edits: CodeEditGroup[] = [];
        const editor = new ImportEditor(new SimpleImportBlockFormatter());
        const fromModuleSpecifier = assertAbsolute(fromFilename).replace(/\.tsx?/g, "") as ModuleSpecifier;
        const toModuleSpecifier = assertAbsolute(toFilename).replace(/\.tsx?/g, "") as ModuleSpecifier;
        const fileNames = await context.getProject().getFileNames();

        for (let file of fileNames) {
            const sourceFile = await context.getSourceFileFor(file);
            const fileEdits = rewriteSymbolImportInFile(
                sourceFile,
                editor,
                fromModuleSpecifier,
                toModuleSpecifier,
                symbolName
            );

            if (fileEdits != null) {
                edits.push(fileEdits);
            }
        }

        return edits;
    }
}
