import { Command } from "./Command";

export interface Response<T> {
    seq: number;
    type: string;
    command: string;
    request_seq: number;
    success: boolean;
    message?: string;
    body?: T;
}

export function getBlankResponseForCommand(command: Command): Response<any> {
    return {
        command: command.command,
        request_seq: command.seq,
        success: undefined!,
        seq: undefined!,
        type: "response"
    };
}

export function getErrorOutputForCommand(command: Command, error: Error): Response<string> {
    let response: Response<string> = getBlankResponseForCommand(command);

    response.success = false;
    response.message = "" + error;

    return response;
}

export function getOutputForCommand<T>(command: Command, body: T): Response<T> {
    let response: Response<T> = getBlankResponseForCommand(command);

    response.success = true;
    response.body = body;
    response.seq = 1;

    return response;
}
