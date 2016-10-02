import { Response } from "../Response";
import log from "../log";

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
