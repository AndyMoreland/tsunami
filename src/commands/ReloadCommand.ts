import * as Bluebird from "bluebird";
import * as fs from "fs";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";

const readFile = Bluebird.promisify(fs.readFile);

export interface ReloadCommand extends Command {
    arguments: {
        file: string;
        tmpfile: string;
    };
}

export class ReloadCommandDefinition implements CommandDefinition<ReloadCommand, void> {
    public predicate(command: Command): command is ReloadCommand {
        return command.command === "reload";
    }

    public async processor(context: TsunamiContext, command: ReloadCommand): Promise<void> {
        const tmp = command.arguments.tmpfile;
        const text = tmp != null ? (await readFile(tmp)).toString() : undefined;
        return context.reloadFile(command.arguments.file, text);
    }
}
