import * as Bluebird from "bluebird";
import * as fs from "fs";
import * as glob from "glob";
import { CodeEdit } from "../protocol/types";
import { Response } from "../Response";
import log from "../log";

const readFilePromise = Bluebird.promisify(fs.readFile);
const writeFilePromise = Bluebird.promisify<void, string, string>(fs.writeFile as any);

export function writeOutput<T>(stream: NodeJS.WritableStream, response: Response<T>) {
    let output = JSON.stringify(response);
    let outputLength = output.length;

    log(output);

    stream.write("Content-Length: " + outputLength + "\n\n");
    stream.write(output + "\n");

    return Bluebird.resolve<void>(null!);
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

export function applyCodeEditsInMemory(fileContents: string, sortedCodeEdits: CodeEdit[]): string {
    let result = fileContents;
    const lineIndex = getLineStartIndex(result);

    sortedCodeEdits.slice().reverse().forEach(edit => {
        const startPos = lineIndex[edit.start.line - 1] + edit.start.offset - 1;
        const endPos = lineIndex[edit.end.line - 1] + edit.end.offset - 1;

        const firstSegment = result.slice(0, startPos);
        const secondSegment = result.slice(endPos);

        result = firstSegment + (edit.newText || "") + secondSegment;
    });

    return result;
}

/**
 * Assumes codeEdits are sorted end-to-start of file.
 * Return true if changes were made.
 */
export async function applyCodeEdits(path: string, sortedCodeEdits: CodeEdit[]): Bluebird<boolean> {
    return readFilePromise(path).then(async buffer => {
        const original = buffer.toString();
        const modified = applyCodeEditsInMemory(original, sortedCodeEdits);
        await writeFilePromise(path, modified);

        return modified !== original;
    });
}

export function globPromise(pattern: string, options: glob.IOptions): Promise<string[]> {
    return new Bluebird<string[]>((resolve, reject) => {
        glob(pattern, options, (err, matches) => {
            if (err) {
                reject(err);
                return;
            }

            resolve(matches);
        });
    });
}
