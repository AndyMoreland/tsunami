import { ChangeCommand, ChangeCommandDefinition } from "../commands/ChangeCommand";
import { FetchSymbolLocationsDefinition } from "../commands/FetchSymbolLocations";
import { GetContainingExpressionsDefinition } from "../commands/GetContainingExpressions";
import { GetContainingScopesDefinition } from "../commands/GetContainingScopes";
import { GetPropertiesOfSymbolDefinition } from "../commands/GetPropertiesOfSymbol";
import { ImplementInterfaceCommandDefinition } from "../commands/ImplementInterface";
import { MoveSymbolCommandDefinition } from "../commands/MoveSymbolCommand";
import { OrganizeImportsCommandDefinition } from "../commands/OrganizeImportsCommand";
import { ReloadCommandDefinition } from "../commands/ReloadCommand";
import { SaveToCommandDefinition } from "../commands/SaveToCommand";
import { buildFormatOptions } from "../formatting/FormatOptions";
import { ChangeCommandTransformer } from "../transformers/ChangeCommandTransformer";
import { Tsunami } from "../Tsunami";
import log, { logWithCallback } from "../log";
import { TsProject } from "../tsProject";

/* HACK */
process.on("uncaughtException", (err: any) => {
    logWithCallback((e: any, data: any) => process.exit(), err);
});

const terminalCommandDefinitions = [
    new FetchSymbolLocationsDefinition(),
    new OrganizeImportsCommandDefinition(),
    new GetContainingExpressionsDefinition(),
    new GetContainingScopesDefinition(),
    new GetPropertiesOfSymbolDefinition(),
    new MoveSymbolCommandDefinition(),
    new ImplementInterfaceCommandDefinition()
];

const nonterminalCommandDefinitions = [
    new ReloadCommandDefinition(),
    new ChangeCommandDefinition(),
    new SaveToCommandDefinition()
];

const transformers = [
    new ChangeCommandTransformer()
];

let projectConfig = process.cwd();

log("Attempting to start server.");
try {
    TsProject.fromRootDir(projectConfig)
        .then(async project => {
            log("Constructing tsunami");
            const tsunami = new Tsunami(
                project,
                buildFormatOptions(),
                { namespaceAliases: new Map() },
                terminalCommandDefinitions,
                nonterminalCommandDefinitions,
                transformers
            );
            await tsunami.initialize();
            log("Done with .initialize");
        }).catch(e => log(e));
} catch (e) {
    log("Failed to start server.", e);
}
