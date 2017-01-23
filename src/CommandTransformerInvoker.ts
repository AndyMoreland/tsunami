import { Command } from "./Command";

export interface CommandTransformerInvoker {
    canTransform(command: Command): boolean;
    transform(command: Command): Promise<Command>;
}
