import { getScopesContainingPoint } from "../ScopeTree";
import { getNodesContainingPoint } from "../utilities/languageUtilities";
import * as ts from "typescript";
import { TsunamiContext } from "../Context";
import log from "../log";
import { Command, CommandDefinition } from "../Command";

export interface GetPropertiesOfSymbolCommand extends Command {
    arguments: {
        file: string;
        line: number;
        offset: number;
    };
}

export class GetPropertiesOfSymbolDefinition implements CommandDefinition<GetPropertiesOfSymbolCommand, ts.Symbol[]> {
    public predicate(command: Command): command is GetPropertiesOfSymbolCommand {
        return command.command === "GET_PROPERTIES_OF_SYMBOL";
    }

    public processor(context: TsunamiContext, command: GetPropertiesOfSymbolCommand): Promise<ts.Symbol[]> {
        const { line, offset, file } = command.arguments;

        return context.getSourceFileFor(file).then(sourceFile => {
            const program = ts.createProgram([sourceFile.fileName], context.getProject().getCompilerOptions());
            const position = sourceFile.getPositionOfLineAndCharacter(line, offset);
            const expressions = getNodesContainingPoint(sourceFile, position);
            const scopes = getScopesContainingPoint(sourceFile, position);
            const typeChecker = program.getTypeChecker();
            const node = expressions[expressions.length - 1];
            log("Nodes text: ", JSON.stringify(node.getText(), null, 2));
            const symbol = typeChecker.getSymbolAtLocation(node);
            const inScope = typeChecker.getSymbolsInScope(
                scopes[scopes.length - 1],
                ts.SymbolFlags.Type | ts.SymbolFlags.Value | ts.SymbolFlags.Namespace | ts.SymbolFlags.Alias
            );
            log("Symbol name: ", (symbol && symbol.getName()) || "No symbol");
            if (inScope) {
                inScope.forEach(symbol => log("in scope: ", symbol.getName()));
            }
            // log(typeChecker.getTypeOfSymbolAtLocation(typeChecker.getSymbolAtLocation(node), node));

            /* const properties = typeChecker.getPropertiesOfType(typeChecker.getTypeAtLocation(node)); */
            return [];
        }).catch(e => {
            log("Error occurred getting properties of symbol: ", e, e.stack);
            log(line, offset, file);
            throw e;
        });
    }
};
