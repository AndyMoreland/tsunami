import { CommandDefinition, Command } from "../Command";
import { TsunamiContext } from "../Context";
import { getBlankResponseForCommand, Response } from "../Response";
import { ImportSorter } from "../importSorter";
import log from "../log";

export interface OrganizeImportsCommand extends Command {
    arguments: {
        filename: string;
    };
}

export class OrganizeImportsCommandDefinition implements CommandDefinition<OrganizeImportsCommand> {
    public predicate(command: Command): command is OrganizeImportsCommand {
        return command.command === "ORGANIZE_IMPORTS";
    }

    public processor(context: TsunamiContext, command: OrganizeImportsCommand): Promise<void> {
        let response: Response<string> = getBlankResponseForCommand(command);
        response.seq = 1;

        return context.updateSourceFileFor(command.arguments.filename).then(sourceFile => {
            let importSorter = new ImportSorter(sourceFile);
            importSorter.sortFileImports((err?: Error) => {
                if (err) { log(err); }
                response.body = "" + err;
                response.success = true;
                context.writeOutput(response);
            });
        }).catch(e => {
            log(e);
            response.body = "" + e;
            context.writeOutput(response);
        });
    }

}
