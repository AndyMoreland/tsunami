import { RegionSpan } from "../protocol/types";
import { getErrorOutputForCommand, Response, getBlankResponseForCommand } from "../Response";
import { getExpressionsContainingPoint } from "../ExpressionTree";
import { TsunamiContext } from "../Context";
import log from "../log";
import { Command, CommandDefinition } from "../Command";

export interface GetContainingExpressionsCommand extends Command {
    arguments: {
        file: string;
        line: number;
        offset: number;
    };
}

export class GetContainingExpressionsDefinition implements CommandDefinition<GetContainingExpressionsCommand> {
    public predicate(command: Command): command is GetContainingExpressionsCommand {
        return command.command === "GET_CONTAINING_EXPRESSIONS";
    }

    public processor(context: TsunamiContext, command: GetContainingExpressionsCommand): Promise<void> {
        const { line, offset, file } = command.arguments;

        return context.getSourceFileFor(file).then(sourceFile => {
            const response: Response<RegionSpan[]> = getBlankResponseForCommand(command);
            const position = sourceFile.getPositionOfLineAndCharacter(line, offset);
            const expressions = getExpressionsContainingPoint(sourceFile, position);
            response.success = true;
            response.body = expressions.map(expr => ({ start: expr.getStart(), end: expr.getEnd()}));
            context.writeOutput(response);
        }).catch(e => {
            log("Error occurred containing getting expressions: ", e, e.stack);
            context.writeOutput(getErrorOutputForCommand(command, e));
        });
    }
};
