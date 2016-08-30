import { TsunamiContext } from "./Context";
import { Command } from "./Command";

export interface CommandInvoker {
    isInvokableCommand(command: Command): boolean;
    invoke(context: TsunamiContext, command: Command): Promise<void>;
}
