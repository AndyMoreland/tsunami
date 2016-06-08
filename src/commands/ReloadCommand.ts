import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";
export interface ReloadCommand extends Command {
    arguments: {
        file: string;
        tmpfile: string;
    };
}


export class ReloadCommandDefinition implements CommandDefinition<ReloadCommand> {
    public predicate(command: Command): command is ReloadCommand {
        return command.command === "reload";
    }

    public processor(context: TsunamiContext, command: ReloadCommand): Promise<void> {
        return context.reloadFile(command.arguments.file, command.arguments.tmpfile);
    }
}
