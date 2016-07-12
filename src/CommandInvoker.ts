import { Command, CommandDefinition } from "./Command";
import { TsunamiContext } from "./Context";
import { getOutputForCommand, getErrorOutputForCommand } from "./Response";
import log from "./log";

export class CommandInvoker {
    constructor(
        private commandDefinitions: CommandDefinition<any, any>[]
    ) {}

    public isInvokableCommand(command: Command): boolean {
        return !!this.commandDefinitions.filter(def => def.predicate(command))[0];
    }

    public invoke(context: TsunamiContext, command: Command): Promise<void> {
        return this.commandDefinitions.filter(def => def.predicate(command))[0]
            .processor(context, command)
            .then(response => {
                if (response != null) {
                    context.writeOutput(getOutputForCommand(command, response));
                }
            })
            .catch(error => {
                log("Failed while executing command: ", command, " error: ", error.stack);
                context.writeOutput(getErrorOutputForCommand(command, error));
            });
    }
}
