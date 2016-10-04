import { CodeEdit } from "../protocol/types";
import { Response } from "../Response";
import log from "../log";
import * as Promise from "bluebird";
import * as fs from "fs";

const readFilePromise = Promise.promisify(fs.readFile);
const writeFilePromise = Promise.promisify<void, string, string>(fs.writeFile);

export function writeOutput<T>(stream: NodeJS.WritableStream, response: Response<T>) {
    let output = JSON.stringify(response);
    let outputLength = output.length;

    log(output);

    stream.write("Content-Length: " + outputLength + "\n\n");
    stream.write(output + "\n");

    return Promise.resolve<void>(null!);
}

export function writeOutputToStdOut<T>(response: Response<T>) {
    return writeOutput(process.stdout, response);
}

/** Line x (0-indexed) starts at character return[x] */
function getLineStartIndex(file: string): number[] {
    let index = -1 ;
    const indices: number[] = [0];

    // tslint:disable-next-line
    while ((index = file.indexOf("\n", index + 1)) >= 0) {
        indices.push(index + 1); /* + 1 to account for the \n */
    }

    return indices;
}

/* Assumes codeEdits are sorted end-to-start of file. */
export function applyCodeEdits(path: string, sortedCodeEdits: CodeEdit[]): Promise<void> {
    return readFilePromise(path).then(buffer => {
        const file = buffer.toString();
        const lineIndex = getLineStartIndex(file);

        let result = file;

        sortedCodeEdits.slice().reverse().forEach(edit => {
            const startPos = lineIndex[edit.start.line - 1] + edit.start.offset - 1;
            const endPos = lineIndex[edit.end.line - 1] + edit.end.offset - 1;

            const firstSegment = file.slice(0, startPos);
            const secondSegment = file.slice(endPos);

            result = firstSegment + (edit.newText || "") + secondSegment;
        });

        return writeFilePromise(path, result);
    });
}
