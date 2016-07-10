import log from "../log";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";

export interface ChangeCommand extends Command {
    arguments: {
        file: string;
        line: number;
        endLine: number;
        offset: number;
        endOffset: number;
        insertString: string;
    };
}

export class ChangeCommandDefinition implements CommandDefinition<ChangeCommand> {
    public predicate(command: Command): command is ChangeCommand {
        return command.command === "change";
    }

    public processor(context: TsunamiContext, command: ChangeCommand): Promise<void> {
        log("Got change request: ", JSON.stringify(command, null, 2));
        return Promise.resolve(null);
    }
}
