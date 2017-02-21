import * as fs from "fs";
import * as path from "path";
import { ImportEditor } from "../imports/ImportEditor";
import { AbsoluteFilename, ModuleSpecifier } from "../imports/ImportStatement";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { CodeEditGroup } from "../protocol/types";
import { appendNode, withoutNode } from "../utilities/editUtils";
import { findNodeForSymbolName, rewriteSymbolImportInFile } from "../utilities/moveSymbolUtils";
import { assertAbsolute } from "../utilities/validation";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";

export interface MoveSymbolCommand extends Command {
    arguments: {
        fromFilename: string;
        toFilename: string;
        symbolName: string;
    };
}

export class MoveSymbolCommandDefinition implements CommandDefinition<MoveSymbolCommand, CodeEditGroup[]> {
    public predicate(command: Command): command is MoveSymbolCommand {
        return command.command === "MOVE_SYMBOL";
    }

    public async processor(context: TsunamiContext, command: MoveSymbolCommand): Promise<CodeEditGroup[]> {
        const { fromFilename, toFilename, symbolName } = command.arguments;

        if (fromFilename === toFilename) {
            throw new Error("Can't move symbol from file to itself.");
        }

        if (!fs.existsSync(fromFilename)) {
            throw new Error(`File ${fromFilename} does not exist`);
        }

        if (!fs.existsSync(toFilename)) {
            throw new Error(`File ${toFilename} does not exist`);
        }

        const edits: CodeEditGroup[] = [];
        const editor = new ImportEditor(SimpleImportBlockFormatter.withDefaultOptions());
        const fromModuleSpecifier = assertAbsolute(fromFilename).replace(/\.tsx?/g, "") as ModuleSpecifier;
        const toModuleSpecifier = assertAbsolute(toFilename).replace(/\.tsx?/g, "") as ModuleSpecifier;
        const fileNames = await context.getProject().getFileNames();

        const sourceFilePromises = fileNames.map(file => context.getSourceFileFor(file));

        for (let sourceFilePromise of sourceFilePromises) {
            const sourceFile = await sourceFilePromise;
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

        const fromSourceFile = await context.getSourceFileFor(fromFilename);
        const toSourceFile = await context.getSourceFileFor(toFilename);

        const symbolNode = findNodeForSymbolName(fromSourceFile, symbolName);
        if (symbolNode != null) {
            edits.unshift({
                file: fromFilename,
                edits: [withoutNode(fromSourceFile, symbolNode)]
            });
            edits.unshift({
                file: toFilename,
                edits: [appendNode(toSourceFile, symbolNode)]
            });
        }

        return edits;
    }
}
