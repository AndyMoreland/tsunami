import * as Promise from "bluebird";
import { SymbolLocation } from "../protocol/types";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";
import { DefinitionType } from "../Indexer";

export interface FetchSymbolLocationsCommand extends Command {
    arguments: {
        prefix: string;
    };
}

export interface FetchSymbolLocationsResponseBody {
    symbolLocations: SymbolLocation[];
};

export class FetchSymbolLocationsDefinition implements CommandDefinition<FetchSymbolLocationsCommand, FetchSymbolLocationsResponseBody> {
    public predicate(command: Command): command is FetchSymbolLocationsCommand {
        return command.command === "SYMBOL_LOCATIONS";
    }

    public processor(context: TsunamiContext, command: FetchSymbolLocationsCommand): Promise<FetchSymbolLocationsResponseBody> {
        try {
            let symbolLocations: SymbolLocation[] = [];

            context.fileIndexerMap.forEach(indexer => {
                indexer.getDefinitionIndex().forEach((definition, symbolName) => {
                        const symbolLocation = {
                            name: symbolName,
                            type: DefinitionType[definition.type],
                            location: {
                                filename: definition.moduleSpecifier,
                                span: definition.span
                            },
                            default: definition.default
                        };
                        symbolLocations.push(symbolLocation);
                    });
            });

            context.moduleIndexerMap.forEach((indexer, moduleName) => {
                indexer.getDefinitionIndex().forEach((definition, symbolName) => {
                        const symbolLocation = {
                            name: symbolName,
                            type: DefinitionType[definition.type],
                            location: {
                                filename: definition.moduleSpecifier,
                                span: definition.span,
                                isExternalModule: true
                            },
                            default: definition.default
                        };
                        symbolLocations.push(symbolLocation);
                    });
            });

            return Promise.resolve({
                symbolLocations
            });
        } catch (e) {
            return Promise.reject(e);
        }
    }
}
