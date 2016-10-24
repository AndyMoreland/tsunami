import { ImportRecord, ModuleSpecifier } from "./ImportStatement";

export type ImportRecords = { [canonicalModuleName: string]: ImportRecord };

export class ImportBlock {
    /* Don't use unless you know what you are doing. */
    constructor(public readonly importRecords: ImportRecords = {}) {}

    /** Attempts to resolve aliases for `symbolName`, if any are noted. */
    public getCurrentName(moduleSpecifier: ModuleSpecifier, symbolName: string): string {
        if (this.importRecords[moduleSpecifier]) {
            const importedAs = this.importRecords[moduleSpecifier].importClause.namedBindings.find(x => x.symbolName === symbolName);
            if (importedAs) {
                return importedAs.alias || importedAs.symbolName;
            }
        }

        return symbolName;
    }

    /** Returns true if this ImportBlock may contain the specified symbol */
    public mayContainImport(moduleSpecifier: ModuleSpecifier, symbolName: string): boolean {
        const specifier = this.importRecords[moduleSpecifier];

        if (!specifier) {
            return false;
        }

        return specifier.importClause.namedBindings.find(x => x.symbolName === symbolName) != null ||
            this.importRecords[moduleSpecifier].namespaceImport != null;
    }
}
