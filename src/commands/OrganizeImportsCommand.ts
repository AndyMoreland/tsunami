import * as Promise from "bluebird";
import { CodeEdit } from "../protocol/types";
import { CommandDefinition, Command } from "../Command";
import { TsunamiContext } from "../Context";
import { getBlankResponseForCommand, Response } from "../Response";
import { ImportSorter } from "../importSorter";

export interface OrganizeImportsCommand extends Command {
    arguments: {
        filename: string;
    };
}

export class OrganizeImportsCommandDefinition implements CommandDefinition<OrganizeImportsCommand, CodeEdit> {
    public predicate(command: Command): command is OrganizeImportsCommand {
        return command.command === "ORGANIZE_IMPORTS";
    }

    public processor(context: TsunamiContext, command: OrganizeImportsCommand): Promise<CodeEdit> {
        let response: Response<string> = getBlankResponseForCommand(command);
        response.seq = 1;
        response.success = true;

        return context.updateSourceFileFor(command.arguments.filename).then(sourceFile => {
            let importSorter = new ImportSorter(sourceFile);
            return importSorter.getSortFileImports();
        });
    }
}
