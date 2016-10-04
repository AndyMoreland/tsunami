import * as Promise from "bluebird";
import { ImportBlock } from "../imports/ImportBlock";
import { ImportEditor } from "../imports/ImportEditor";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { CodeEdit } from "../protocol/types";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";

export interface OrganizeImportsCommand extends Command {
    arguments: {
        filename: string;
    };
}

export class OrganizeImportsCommandDefinition implements CommandDefinition<OrganizeImportsCommand, CodeEdit[] | null> {
    public predicate(command: Command): command is OrganizeImportsCommand {
        return command.command === "ORGANIZE_IMPORTS";
    }

    public processor(context: TsunamiContext, command: OrganizeImportsCommand): Promise<CodeEdit[] | null> {
        return context.getSourceFileFor(command.arguments.filename).then(sourceFile => {
            const editor = new ImportEditor(new SimpleImportBlockFormatter());
            const importBlock = ImportBlock.fromFile(sourceFile);
            return editor.applyImportBlockToFile(sourceFile, importBlock);
        });
    }
}
