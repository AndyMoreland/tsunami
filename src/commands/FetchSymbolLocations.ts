import * as Promise from "bluebird";
import { SymbolLocation } from "../protocol/types";
import { CommandDefinition, Command } from "../Command";
import { TsunamiContext } from "../Context";
import { DefinitionType } from "../Indexer";
import { Response, getBlankResponseForCommand } from "../Response";

export interface FetchSymbolLocationsCommand extends Command {
    arguments: {
        prefix: string;
    };
}

export interface FetchSymbolLocationsResponseBody {
    symbolLocations: SymbolLocation[];
};

export class FetchSymbolLocationsDefinition implements CommandDefinition<FetchSymbolLocationsCommand> {
    public predicate(command: Command): command is FetchSymbolLocationsCommand {
        return command.command === "SYMBOL_LOCATIONS";
    }

    public processor(context: TsunamiContext, command: FetchSymbolLocationsCommand): Promise<void> {
        let response: Response<FetchSymbolLocationsResponseBody> = getBlankResponseForCommand(command);
        let symbolLocations: SymbolLocation[] = [];

        Object.keys(context.fileIndexerMap).forEach(filename => {
            let definitions = context.fileIndexerMap[filename].getDefinitionIndex();
            Object.keys(definitions).forEach(
                symbolName => {
                    let definition = definitions[symbolName];
                    let symbolLocation =  {
                        name: symbolName,
                        type: DefinitionType[definition.type],
                        location: {
                            filename: definition.filename,
                            pos: definition.location
                        },
                        default: definition.default
                    };
                    symbolLocations.push(symbolLocation);
                });
        });

        Object.keys(context.moduleIndexerMap).forEach(moduleName => {
            let definitions = context.moduleIndexerMap[moduleName].getDefinitionIndex();
            Object.keys(definitions).forEach(
                symbolName => {
                    let definition = definitions[symbolName];
                    let symbolLocation = {
                        name: symbolName,
                        type: DefinitionType[definition.type],
                        location: {
                            filename: moduleName,
                            pos: definition.location,
                            isExternalModule: true
                        },
                        default: definition.default
                    };
                    symbolLocations.push(symbolLocation);
                });
        });

        response.seq = 1;
        response.success = true;
        response.body = {
            symbolLocations: symbolLocations
        };

        return context.writeOutput(response) as any as Promise<void>;
    }
}
