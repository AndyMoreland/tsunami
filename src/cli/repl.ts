#!/usr/bin/env node

import * as path from "path";
import * as fs from "fs";
import * as tsu from "../index";
import * as readline from "readline";

async function startRepl() {
    const projectRoot = process.argv[2];
    console.log("Starting in: ", projectRoot);
    const settings = JSON.parse(fs.readFileSync(path.join(projectRoot, "tsconfig.json")).toString());
    const tsunami = new tsu.Tsunami(
        new tsu.TsProject(projectRoot, settings)
    );

    await tsunami.buildInitialProjectIndex();

    console.log("Built initial index.");

    return tsunami;
}

async function main() {
    console.log("Starting main.");
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

    const prompt = (query: string) => new Promise<string>((resolve, reject) => rl.question(query, resolve));

    try {
        const context = (await startRepl()).getContext();

        while (true) {
            const command = await prompt("> ");
            console.log("Searching symbols matchings: ", command);
            const start = process.hrtime();
            const results = await context.getMatchingSymbols(command);
            const diff = process.hrtime(start);
            results.slice(0, 10).map(it => it.text).forEach(it => {
                console.log(it);
            });
            console.log(`in ${(diff[0] * 1e9 + diff[1]) / 1e6} milliseconds`);
        }
    } catch (e) {
        console.error(e);
    }
}

main();
