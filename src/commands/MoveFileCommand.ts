import * as fs from "fs";
import * as path from "path";
import { ImportEditor } from "../imports/ImportEditor";
import { AbsoluteFilename, ModuleSpecifier } from "../imports/ImportStatement";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { CodeEditGroup } from "../protocol/types";
import { compact } from "../utilities/collectionUtilities";
import { appendNode, withoutNode } from "../utilities/editUtils";
import { findNodeForSymbolName, rewriteModuleImportInFile, rewriteSymbolImportInFile } from "../utilities/moveSymbolUtils";
import { assertAbsolute } from "../utilities/validation";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";

export interface MoveFileCommand extends Command {
    arguments: {
        fromFilename: string;
        toFilename: string;
    };
}

export class MoveFileCommandDefinition implements CommandDefinition<MoveFileCommand, CodeEditGroup[]> {
    public predicate(command: Command): command is MoveFileCommand {
        return command.command === "MOVE_FILE";
    }

    public async processor(context: TsunamiContext, command: MoveFileCommand): Promise<CodeEditGroup[]> {
        const { fromFilename, toFilename } = command.arguments;

        if (fromFilename === toFilename) {
            throw new Error("Can't move file from file to itself.");
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

        const codeEditPromises = fileNames.map(async file => {
            const sourceFile = await context.getSourceFileFor(file);

            const fileEdits = rewriteModuleImportInFile(
                sourceFile,
                editor,
                fromModuleSpecifier,
                toModuleSpecifier,
            );

            return fileEdits;
        });

        const codeEditGroups = await Promise.all(codeEditPromises);

        return compact(codeEditGroups);
    }
}
