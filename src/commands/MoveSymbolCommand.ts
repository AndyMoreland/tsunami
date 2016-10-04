import log from "../log";
import { ImportEditor } from "../imports/ImportEditor";
import { AbsoluteFilename, ModuleSpecifier } from "../imports/ImportStatement";
import { ImportBlock } from "../imports/ImportBlock";
import { ImportBlockBuilder } from "../imports/ImportBlockBuilder";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import * as Promise from "bluebird";
import { CodeEditGroup } from "../protocol/types";
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

export class MoveSymbolCommandDefinition implements CommandDefinition<MoveSymbolCommand, CodeEditGroup[] | null> {
    public predicate(command: Command): command is MoveSymbolCommand {
        return command.command === "MOVE_SYMBOL";
    }

    public processor(context: TsunamiContext, command: MoveSymbolCommand): Promise<CodeEditGroup[] | null> {
        const { fromFilename, toFilename, symbolName } = command.arguments;
        const editor = new ImportEditor(new SimpleImportBlockFormatter());

        const fromModuleSpecifier = assertAbsolute(fromFilename).replace(/\.tsx?/g, "") as ModuleSpecifier;
        const toModuleSpecifier = assertAbsolute(toFilename).replace(/\.tsx?/g, "") as ModuleSpecifier;

        return context.getProject().getFileNames().then(fileNames => {
            const promises = fileNames.map(file => {
                return context.getSourceFileFor(file).then(sourceFile => {
                    const currentBlock = ImportBlock.fromFile(sourceFile);
                    if (currentBlock.mayContainImport(fromModuleSpecifier, symbolName)) {
                        const currentName = currentBlock.getCurrentName(fromModuleSpecifier, symbolName);
                        const newBlock = ImportBlockBuilder.from(currentBlock)
                            .withoutImport(fromModuleSpecifier, symbolName)
                            .addImportBinding(toModuleSpecifier, {
                                symbolName,
                                alias: currentName !== symbolName ? currentName : undefined
                            })
                            .build();

                        const edits = editor.applyImportBlockToFile(sourceFile, newBlock);
                        return { file, edits };
                    } else {
                        return [];
                    }
                });
            });

            return Promise.all(promises).then(x => ([] as CodeEditGroup[]).concat(...x));
        });
    }
}
