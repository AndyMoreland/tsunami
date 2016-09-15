import { ImportBlock } from "./ImportBlock";
export interface ImportBlockFormatter {
    formatImportBlock(localPath: string, importBlock: ImportBlock): string;
}
