import { InitializedFormatOptions } from "../formatting/FormatOptions";
import { ImportBlock } from "./ImportBlock";

export interface ImportBlockFormatter {
    formatImportBlock(localPath: string, importBlock: ImportBlock): string;
}

export interface ImportBlockFormatterOptions extends InitializedFormatOptions {
}
