import * as Bluebird from "bluebird";
import * as path from "path";
import { SymbolLocation } from "../protocol/types";
import { Command, CommandDefinition } from "../Command";
import { TsunamiContext } from "../Context";
import { DefinitionType } from "../Indexer";
import log from "../log";

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

    public async processor(context: TsunamiContext, command: FetchSymbolLocationsCommand): Bluebird<FetchSymbolLocationsResponseBody> {
        let symbolLocations: SymbolLocation[] = [];

        for (let definition of context.getIndexedDefinitions()) {
            log(definition.moduleSpecifier);
            const symbolLocation = {
                name: definition.text || "",
                type: DefinitionType[definition.type],
                location: {
                    filename: definition.moduleSpecifier,
                    span: definition.span,
                    isExternalModule: !path.isAbsolute(definition.moduleSpecifier)
                },
                default: definition.default
            };

            symbolLocations.push(symbolLocation);
        }

        return Bluebird.resolve({
            symbolLocations
        });
    }
}
