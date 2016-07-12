import { TsunamiContext } from "./Context";

export interface Command {
    command: string;
    seq: number;
    arguments: any;
}

export interface CommandPredicate<C extends Command> {
    (command: Command): command is C;
}

export interface CommandProcessor<C extends Command, R> {
    (context: TsunamiContext, command: C): Promise<R>;
}

export interface CommandDefinition<C extends Command, R> {
    predicate: CommandPredicate<C>;
    processor: CommandProcessor<C, R>;
}
