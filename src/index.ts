/// <reference path="../typings/node/node.d.ts" />
import { Tsunami } from "./Tsunami";
import { ChangeCommandDefinition } from "./commands/ChangeCommand";
import { FetchSymbolLocationsDefinition } from "./commands/FetchSymbolLocations";
import { GetContainingExpressionsDefinition } from "./commands/GetContainingExpressions";
import { GetContainingScopesDefinition } from "./commands/GetContainingScopes";
import { GetPropertiesOfSymbolDefinition } from "./commands/GetPropertiesOfSymbol";
import { MoveSymbolCommandDefinition } from "./commands/MoveSymbolCommand";
import { OrganizeImportsCommandDefinition } from "./commands/OrganizeImportsCommand";
import { ReloadCommandDefinition } from "./commands/ReloadCommand";
import { SaveToCommandDefinition } from "./commands/SaveToCommand";
import log, { logWithCallback } from "./log";
import { TsProject } from "./tsProject";

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
    new MoveSymbolCommandDefinition()
];

const nonterminalCommandDefinitions = [
    new ReloadCommandDefinition(),
    new ChangeCommandDefinition(),
    new SaveToCommandDefinition()
];

let projectConfig = process.cwd();

log("Attempting to start server.");
TsProject.constructFromFilename(projectConfig)
    .then(project => {
        log("Constructing tsunami");
        const tsunami = new Tsunami(project, terminalCommandDefinitions, nonterminalCommandDefinitions);
        tsunami.initialize();
        log("Done with .initialize");
    });
