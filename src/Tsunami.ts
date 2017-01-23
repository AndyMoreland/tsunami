import * as JSONStream from "JSONStream";
import * as p from "child_process";
import * as es from "event-stream";
import * as ts from "typescript";
import { BenchmarkingCommandInvoker } from "./BenchmarkingCommandInvoker";
import { Command, CommandDefinition } from "./Command";
import { CommandInvoker } from "./CommandInvoker";
import { CommandTransformerInvoker } from "./CommandTransformerInvoker";
import { TsunamiContext } from "./Context";
import { MutableTsunamiContext } from "./MutableTsunamiContext";
import { ProjectIndexer } from "./ProjectIndexer";
import { getErrorOutputForCommand } from "./Response";
import { SimpleCommandInvoker } from "./SimpleCommandInvoker";
import { SimpleCommandTransformerInvoker } from "./SimpleCommandTransformerInvoker";
import { ImportConfig } from "./config/ImportConfig";
import { InitializedFormatOptions } from "./formatting/FormatOptions";
import log from "./log";
import { CommandTransformer } from "./transformers/CommandTransformer";
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
    private commandTransformerInvoker: CommandTransformerInvoker;
    private documentRegistry: ts.DocumentRegistry;
    private context: TsunamiContext;
    private projectIndexer: ProjectIndexer;

    constructor(
        private tsProject: TsProject,
        private formatOptions: InitializedFormatOptions,
        private importConfig: ImportConfig,
        terminalCommandDefinitions: CommandDefinition<any, any>[] = [],
        nonterminalCommandDefinitions: CommandDefinition<any, any>[] = [],
        transformers: CommandTransformer<any, any>[] = [],
        context?: TsunamiContext
    ) {
        this.terminalInvoker = new BenchmarkingCommandInvoker(new SimpleCommandInvoker(terminalCommandDefinitions));
        this.nonterminalInvoker = new BenchmarkingCommandInvoker(new SimpleCommandInvoker(nonterminalCommandDefinitions));
        this.documentRegistry = ts.createDocumentRegistry(true);
        this.context = context || new MutableTsunamiContext(
            this.tsProject,
            this.formatOptions,
            this.importConfig,
            writeOutputToStdOut,
            this.documentRegistry
        );
        this.commandTransformerInvoker = new SimpleCommandTransformerInvoker(this.context, transformers);
        this.projectIndexer = new ProjectIndexer(tsProject, this.context);
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
        await this.projectIndexer.indexProject();
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
        } else if (this.commandTransformerInvoker.canTransform(command)) {
            this.commandTransformerInvoker.transform(command).then(newCommand => {
                log("Proxying transformed command: ", JSON.stringify(newCommand, null, 2));
                cb(null, JSON.stringify(newCommand) + "\n");
            });
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
}
