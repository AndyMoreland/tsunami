import log from "../log";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";

export interface SaveToCommand extends Command {
    arguments: {
        file: string;
        tmpfile: string;
    };
}

export class SaveToCommandDefinition implements CommandDefinition<SaveToCommand, void> {
    public predicate(command: Command): command is SaveToCommand {
        return command.command === "saveto";
    }

    public processor(context: TsunamiContext, command: SaveToCommand): Promise<void> {
        log("Got SaveTo request: ", JSON.stringify(command, null, 2));
        return Promise.resolve(null!);
    }
}
