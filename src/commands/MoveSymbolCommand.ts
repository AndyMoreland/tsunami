import * as Promise from "bluebird";
import { ImportEditor } from "../imports/ImportEditor";
import { AbsoluteFilename, ModuleSpecifier } from "../imports/ImportStatement";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { CodeEditGroup } from "../protocol/types";
import { rewriteFile } from "../utilities/moveSymbolUtils";
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

    public processor(context: TsunamiContext, command: MoveSymbolCommand): Promise<CodeEditGroup[] | null> {
        const { fromFilename, toFilename, symbolName } = command.arguments;

        if (fromFilename === toFilename) {
            return Promise.reject(new Error("Can't move symbol from file to itself."));
        }

        try {
            const editor = new ImportEditor(new SimpleImportBlockFormatter());
            const fromModuleSpecifier = assertAbsolute(fromFilename).replace(/\.tsx?/g, "") as ModuleSpecifier;
            const toModuleSpecifier = assertAbsolute(toFilename).replace(/\.tsx?/g, "") as ModuleSpecifier;

            return context.getProject().getFileNames().then(fileNames => {
                const promises = fileNames.map(
                    file => context.getSourceFileFor(file).then(
                        (sourceFile) => rewriteFile(
                            sourceFile,
                            editor,
                            fromModuleSpecifier,
                            toModuleSpecifier,
                            symbolName
                        )));
                promises.push(context.getSourceFileFor(fromFilename).then(
                    (sourceFile) => rewriteFile(
                        sourceFile,
                        editor,
                        fromModuleSpecifier,
                        toModuleSpecifier,
                        symbolName
                    )));
                return Promise.all(promises).then(x => ([] as CodeEditGroup[]).concat(...x));
            });
        } catch (e) {
            return Promise.reject(e);
        }
    }
}
