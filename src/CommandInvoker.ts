import { UnknownObject } from "./types";
import { TsunamiContext } from "./Context";
import { Command, CommandDefinition } from "./Command";

export class CommandInvoker {
    constructor(
        private commandDefinitions: CommandDefinition<any>[]
    ) {}

    public isInvokableCommand(command: Command): boolean {
        return !!this.commandDefinitions.filter(def => def.predicate(command))[0];
    }

    public invoke(context: TsunamiContext, command: Command): Promise<void> {
        return this.commandDefinitions.filter(def => def.predicate(command))[0]
            .processor(context, command);
    }
}
