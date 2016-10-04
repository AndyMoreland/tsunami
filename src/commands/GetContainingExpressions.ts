import { RegionSpan } from "../protocol/types";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";
import { getExpressionsContainingPoint } from "../ExpressionTree";
import log from "../log";

export interface GetContainingExpressionsCommand extends Command {
    arguments: {
        file: string;
        line: number;
        offset: number;
    };
}

export class GetContainingExpressionsDefinition implements CommandDefinition<GetContainingExpressionsCommand, RegionSpan[]> {
    public predicate(command: Command): command is GetContainingExpressionsCommand {
        return command.command === "GET_CONTAINING_EXPRESSIONS";
    }

    public processor(context: TsunamiContext, command: GetContainingExpressionsCommand): Promise<RegionSpan[]> {
        const { line, offset, file } = command.arguments;

        return context.getSourceFileFor(file).then(sourceFile => {
            const position = sourceFile.getPositionOfLineAndCharacter(line, offset);
            const expressions = getExpressionsContainingPoint(sourceFile, position);
            return expressions.map(expr => ({ start: expr.getStart(), end: expr.getEnd()}));
        }).catch(e => {
            log("Error occurred containing getting expressions: ", e, e.stack);
            throw e;
        });
    }
};
