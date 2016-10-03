import * as path from "path";
import { ImportBlock } from "./ImportBlock";
import { ImportBlockFormatter } from "./ImportBlockFormatter";
import { ImportRecord, ImportStatementType } from "./ImportStatement";

/* Distance is in [0, inf]. Smaller distance => closer in the file tree. */
function computeDistance(specifier: string): number {
    if (specifier.slice(0, 2) === "./") {
        return 0;
    }

    const firstChar = specifier.charAt(0);

    /* For imports from dependencies, we want 3rd party dependencies listed before
       internal dependencies. */

    if (firstChar !== ".") {
        if (firstChar === "@") {
            return 5000;
        }

        if (specifier.slice(0, 3) !== "../") {
            return 10000;
        }
    }

    return (specifier.match(/..\//g) || []).length;
}

/** Returns relative path, ensuring it begins with ./ or ../ */
function getPrefixedRelativePath(from: string, to: string): string {
    const result = path.relative(from, to);
    if (!result.startsWith(".")) {
        return "./" + result;
    } else {
        return result;
    }
}

/** Given a working directory, returns a module specifier for importRecord. */
function getLocalModuleSpecifier(localPath: string, importRecord: ImportRecord): string {
    const type = importRecord.type;

    if (type === ImportStatementType.PROJECT_RELATIVE) {
        return getPrefixedRelativePath(localPath, importRecord.moduleSpecifier);
    } else {
        return importRecord.moduleSpecifier;
    }
}

export class SimpleImportBlockFormatter implements ImportBlockFormatter {
    private emitImportRecord(localPath: string, importRecord: ImportRecord): string[] {
        const ret: string[] = [];
        const hasBindings = importRecord.importClause.defaultName != null || importRecord.importClause.namedBindings.length > 0;
        const spec = getLocalModuleSpecifier(localPath, importRecord);

        if (importRecord.namespaceImport == null && !hasBindings)  {
            ret.push(`import "${spec}"`);
        } else {
            if (importRecord.namespaceImport != null) {
                const alias = importRecord.namespaceImport.alias;
                ret.push(`import * as ${alias} from "${spec}"`);
            }

            if (hasBindings) {
                const defaultName = importRecord.importClause.defaultName;
                const sortedBindings = importRecord.importClause.namedBindings.sort((a, b) => (a.symbolName === b.symbolName) ? 0 :
                                                                                    a.symbolName >= b.symbolName ? 1 : -1);
                const sortedStringBindings = sortedBindings.map(binding => {
                    return [binding.symbolName, binding.alias].filter(x => x != null).join(" as ");
                });

                const symbols = sortedBindings.length > 0 ? `{ ${sortedStringBindings.join(", ")} }` : null;
                const bindings = [defaultName, symbols].filter(x => x != null).join(", ");
                ret.push(`import ${bindings} from "${spec}"`);
            }
        }

        return ret;
    }

    public formatImportBlock(localPath: string, importBlock: ImportBlock): string {
        const moduleSpecifiers = Object.keys(importBlock.importRecords);

        const sortedModuleSpecifiers = moduleSpecifiers.sort((a, b) => {
            const localA = getLocalModuleSpecifier(localPath, importBlock.importRecords[a]);
            const localB = getLocalModuleSpecifier(localPath, importBlock.importRecords[b]);

            const distanceA = computeDistance(localA);
            const distanceB = computeDistance(localB);

            if (distanceA !== distanceB) {
                return distanceA > distanceB ? -1 : 1;
            }

            if (a === b) {
                return 0;
            }

            return a >= b ? 1 : -1;
        });

        return sortedModuleSpecifiers.map(spec => {
            return this.emitImportRecord(localPath, importBlock.importRecords[spec]);
        }).reduce((acc, el) => {
            acc.push(...el);
            return acc;
        }, [] as string[]).join(";\n") + ";\n"; /* Make sure we get the trailing ;\n */
    }
}
