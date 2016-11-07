import { ChangeCommandDefinition } from "../commands/ChangeCommand";
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

let projectConfig = process.cwd();

log("Attempting to start server.");
TsProject.fromRootDir(projectConfig)
    .then(async project => {
        log("Constructing tsunami");
        const tsunami = new Tsunami(project, buildFormatOptions(), terminalCommandDefinitions, nonterminalCommandDefinitions);
        await tsunami.initialize();
        log("Done with .initialize");
    }).catch(e => log(e));
