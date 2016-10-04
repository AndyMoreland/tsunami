import { Command } from "./Command";
import { TsunamiContext } from "./Context";

export interface CommandInvoker {
    isInvokableCommand(command: Command): boolean;
    invoke(context: TsunamiContext, command: Command): Promise<void>;
}
