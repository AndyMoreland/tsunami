import { TsunamiContext } from "./Context";

export interface Command {
    command: string;
    seq: number;
    arguments: any;
}

export interface CommandPredicate<C extends Command> {
    (command: Command): command is C;
}

export interface CommandProcessor<C extends Command> {
    (context: TsunamiContext, command: C): Promise<void>;
}

export interface CommandDefinition<C extends Command> {
    predicate: CommandPredicate<C>;
    processor: CommandProcessor<C>;
}
