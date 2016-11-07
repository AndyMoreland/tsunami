import * as JSONStream from "JSONStream";
import * as Bluebird from "bluebird";
import * as p from "child_process";
import * as es from "event-stream";
import * as fs from "fs";
import * as ts from "typescript";
import { BenchmarkingCommandInvoker } from "./BenchmarkingCommandInvoker";
import { Command, CommandDefinition } from "./Command";
import { CommandInvoker } from "./CommandInvoker";
import { TsunamiContext } from "./Context";
import { FileIndexer } from "./FileIndexer";
import { MutableTsunamiContext } from "./MutableTsunamiContext";
import { getErrorOutputForCommand } from "./Response";
import { SimpleCommandInvoker } from "./SimpleCommandInvoker";
import { InitializedFormatOptions } from "./formatting/FormatOptions";
import { ModuleName } from "./imports/ImportStatement";
import log from "./log";
import { TsProject } from "./tsProject";
import { CallbackFunction, UnknownObject } from "./types";
import { writeOutputToStdOut } from "./utilities/ioUtils";

function parseCommand(data: {[index: string]: any}): Command {
    if (data["command"] !== undefined && data["seq"] !== undefined) {
        return data as Command;
    } else {
        throw new Error("Invalid command format.");
    }
}

export class Tsunami {
    private terminalInvoker: CommandInvoker;
    private nonterminalInvoker: CommandInvoker;
    private documentRegistry: ts.DocumentRegistry;
    private context: TsunamiContext;

    constructor(
        private tsProject: TsProject,
        private formatOptions: InitializedFormatOptions,
        terminalCommandDefinitions: CommandDefinition<any, any>[] = [],
        nonterminalCommandDefinitions: CommandDefinition<any, any>[] = [],
        context?: TsunamiContext
    ) {
        this.terminalInvoker = new BenchmarkingCommandInvoker(new SimpleCommandInvoker(terminalCommandDefinitions));
        this.nonterminalInvoker = new BenchmarkingCommandInvoker(new SimpleCommandInvoker(nonterminalCommandDefinitions));
        this.documentRegistry = ts.createDocumentRegistry(true);
        this.context = context || new MutableTsunamiContext(
            this.tsProject,
            this.formatOptions,
            writeOutputToStdOut,
            this.documentRegistry
        );
    }

    public async initialize(): Promise<void> {
        log("initializing");
        await this.buildRiggedTsServerProcess();
        await this.buildInitialProjectIndex();
        log("initialized");
    }

    public getContext(): TsunamiContext {
        return this.context;
    }

    public buildRiggedTsServerProcess() {
        let tsserver: p.ChildProcess = p.spawn("tsserver");

        process.stdin.resume();
        process.stdin
            .pipe(JSONStream.parse(undefined))
            .pipe(es.map(this.processPotentialTsunamiCommand))
            .pipe(tsserver.stdin);

        tsserver.stdout.pipe(process.stdout);
        tsserver.stdout
            .pipe(es.map((data: any, cb: CallbackFunction<any>) => {
                log(data);
                cb(null, data);
            }));
    }

    public async buildInitialProjectIndex(): Promise<void> {
        await Bluebird.all([
            this.buildInitialInternalProjectIndex(),
            this.indexDependenciesOfProject()
        ]);
    }

    private async indexDependenciesOfProject(): Promise<void> {
        try {
            const deps = await this.tsProject.getDependencyFilenames();
            log("Dependency typings: ", JSON.stringify(deps, null, 2));
            const promises: Promise<void>[] = [];

            Object.keys(deps).forEach(dep => {
                /* indexExternalModule(dep); */
                try {
                    fs.accessSync(deps[dep]);
                    let typings = deps[dep];

                    if (typings) {
                        promises.push(this.indexDefinitionFile(dep, typings));
                    }
                } catch (e) {
                    log("Failed to index: ", dep);
                }
            });

            await Bluebird.all(promises);
        } catch (error) {
            log("Failed to get dependency filenames.");
            log(error.stack);
        }
    }

    private async buildInitialInternalProjectIndex(): Promise<void> {
        const files = await this.tsProject.getFileNames();

        log(JSON.stringify(files, null, 2));
        const promises = files.map(file => this.context.getSourceFileFor(file).then(() => {
            log("Indexing: ", file);
            return this.context.reloadFile(file);
        }));

        await Bluebird.all(promises);
        log("Finished starting server.");
    }

    private processPotentialTsunamiCommand = (data: UnknownObject, cb: CallbackFunction<string>) => {
        // log("Incoming command: ", JSON.stringify(data, null, 2));
        let command = parseCommand(data);

        if (this.terminalInvoker.isInvokableCommand(command)) {
            // log("Processing command with tsunami.");
            try {
                this.terminalInvoker.invoke(this.context, command);
            } catch (e) {
                /* Prevents propagation of command to tsserver.stdin */
                log("Error processing tsunami command: ", JSON.stringify(command, null, 2), e.stack);
                writeOutputToStdOut<string>(getErrorOutputForCommand(command, e));
            }
            cb();
        } else {
            if (this.nonterminalInvoker.isInvokableCommand(command)) {
                // log("Wiretapping command with tsunami.");
                try {
                    this.nonterminalInvoker.invoke(this.context, command);
                } catch (e) {
                    log("Error wiretapping tsunami command: ", JSON.stringify(command, null, 2), e);
                }
            }
            // log("Proxying to tsserver: ", JSON.stringify(command, null, 2));
            /* Pass the re-string-form'd object straight through. */
            cb(null, JSON.stringify(command) + "\n");
        }
    }

    private async indexDefinitionFile(moduleName: string, filename: string): Promise<void> {
        await this.context.getSourceFileFor(filename);
        const sourceFile = await this.context.updateSourceFileFor(filename); // FIXME: Why?????????
        log("Indexing definition file:", sourceFile.fileName, moduleName);
        const indexer = new FileIndexer(
            moduleName as ModuleName,
            sourceFile,
            fileName => this.context.getSourceFileFor(fileName)
        );
        this.context.moduleIndexerMap.set(moduleName, indexer);
        return indexer.indexFile();
    }
}
