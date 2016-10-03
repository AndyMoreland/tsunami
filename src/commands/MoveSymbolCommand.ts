import { ImportEditor } from "../imports/ImportEditor";
import { AbsoluteFilename, ModuleSpecifier } from "../imports/ImportStatement";
import { ImportBlock } from "../imports/ImportBlock";
import { ImportBlockBuilder } from "../imports/ImportBlockBuilder";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import * as Promise from "bluebird";
import { CodeEditForFile } from "../protocol/types";
import { CommandDefinition, Command } from "../Command";
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

export class MoveSymbolCommandDefinition implements CommandDefinition<MoveSymbolCommand, CodeEditForFile[] | null> {
    public predicate(command: Command): command is MoveSymbolCommand {
        return command.command === "ORGANIZE_IMPORTS";
    }

    public processor(context: TsunamiContext, command: MoveSymbolCommand): Promise<CodeEditForFile[] | null> {
        const { fromFilename, toFilename, symbolName } = command.arguments;
        const editor = new ImportEditor(new SimpleImportBlockFormatter());

        return context.getProject().getFileNames().then(fileNames => {
            const promises = fileNames.map(file => {
                return context.getSourceFileFor(file).then(sourceFile => {
                    const currentBlock = ImportBlock.fromFile(sourceFile);
                    if (currentBlock.mayContainImport(fromFilename as ModuleSpecifier, symbolName)) {
                        const currentName = currentBlock.getCurrentName(assertAbsolute(fromFilename), symbolName);
                        const newBlock = ImportBlockBuilder.from(currentBlock)
                            .withoutImport(assertAbsolute(fromFilename), symbolName)
                            .addImportBinding(assertAbsolute(toFilename), {
                                symbolName,
                                alias: currentName !== symbolName ? currentName : undefined
                            })
                            .build();

                        const edits = editor.applyImportBlockToFile(sourceFile, newBlock);
                        return edits.map(edit => ({ edit, filename: sourceFile.fileName }));
                    } else {
                        return [];
                    }
                });
            });

            return Promise.all(promises).then(x => ([] as CodeEditForFile[]).concat(...x));
        });
    }
}
