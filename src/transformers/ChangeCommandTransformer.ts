import * as ts from "typescript";
import { ChangeCommand } from "../commands/ChangeCommand";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";
import { CommandTransformer } from "./CommandTransformer";

export interface TsunamiChangeCommand extends Command {
    command: string;
    seq: number;
    arguments: {
        file: string;
        startPos: number;
        endPos: number;
        insertString: string;
    };
}

export class TsunamiChangeCommandDefinition implements CommandDefinition<TsunamiChangeCommand, void> {
    predicate(command: Command): command is TsunamiChangeCommand {
        return command.command === "tsunami-change";
    }

    /* unused. */
    processor: any = null;
}

export class ChangeCommandTransformer implements CommandTransformer<TsunamiChangeCommand, ChangeCommand> {
    readonly definition = new TsunamiChangeCommandDefinition();

    async transformCommand(context: TsunamiContext, command: TsunamiChangeCommand): Promise<ChangeCommand> {
        const args = command.arguments;
        const file = await context.getSourceFileFor(args.file);

        const startLocation = ts.getLineAndCharacterOfPosition(file, args.startPos);
        const endLocation = ts.getLineAndCharacterOfPosition(file, args.endPos);

        return {
            arguments: {
                file: args.file,
                insertString: args.insertString,
                line: startLocation.line + 1,
                offset: startLocation.character,
                endLine: endLocation.line + 1,
                endOffset: endLocation.character,
            },
            command: "change",
            seq: command.seq
        };
    }
}
