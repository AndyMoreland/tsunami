import * as path from "path";
import { AbsoluteFilename } from "../imports/ImportStatement";

export function assertAbsolute(filename: string): AbsoluteFilename {
    if (!path.isAbsolute(filename)) {
        throw new Error("Must pass absolute filename.");
    }

    return filename as AbsoluteFilename;
}
