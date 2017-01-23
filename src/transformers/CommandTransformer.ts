import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";

export interface CommandTransformer<T extends Command, U extends Command> {
    definition: CommandDefinition<T, any>;
    transformCommand(context: TsunamiContext, command: T): Promise<U>;
}
