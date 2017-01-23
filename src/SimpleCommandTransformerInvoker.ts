import { TsunamiContext } from "./Context";
import log from "./log";
import { Command } from "./Command";
import { CommandTransformerInvoker } from "./CommandTransformerInvoker";
import { CommandTransformer } from "./transformers/CommandTransformer";

export class SimpleCommandTransformerInvoker implements CommandTransformerInvoker {
    constructor(
        private context: TsunamiContext,
        private transformers: CommandTransformer<any, any>[]
    ) {}

    canTransform(command: Command): boolean {
        for (let transformer of this.transformers) {
            if (transformer.definition.predicate(command)) {
                return true;
            }
        }

        return false;
    }

    async transform(command: Command): Promise<Command> {
        for (let transformer of this.transformers) {
            if (transformer.definition.predicate(command)) {
                try {
                    return transformer.transformCommand(this.context, command);
                } catch (e) {
                    log("Failed to transform command: " + command.command + " with exception: " + e);
                    return command;
                }
            }
        }

        throw new Error("Failed to find transformer for command: " + command.command);
    }
}
