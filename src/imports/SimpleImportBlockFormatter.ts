import * as path from "path";
import { ImportBlock } from "./ImportBlock";
import { ImportBlockFormatter } from "./ImportBlockFormatter";
import { ImportRecord, ImportStatementType, getTypeOfModuleSpecifier, ModuleSpecifier } from "./ImportStatement";

/* Distance is in [0, inf]. Smaller distance => closer in the file tree. */
function computeDistance(specifier: string): number {
    if (specifier.slice(1, 3) === "./") {
        return 0;
    }

    const firstChar = specifier.charAt(1);

    /* For imports from dependencies, we want 3rd party dependencies listed before
       internal dependencies. */

    if (firstChar !== ".") {
        if (firstChar === "@") {
            return 5000;
        }

        if (specifier.slice(1, 4) !== "../") {
            return 10000;
        }
    }

    return (specifier.match(/..\//g) || []).length;
}

function toLocalModuleSpecifier(localPath: string, moduleSpecifier: string): string {
    const type = getTypeOfModuleSpecifier(moduleSpecifier);

    if (type === ImportStatementType.PROJECT_RELATIVE) {
        return path.relative(localPath, moduleSpecifier);
    } else {
        return moduleSpecifier;
    }
}

export class SimpleImportBlockFormatter implements ImportBlockFormatter {
    private emitImportRecord(localPath: string, importRecord: ImportRecord): string[] {
        const ret: string[] = [];
        const hasBindings = importRecord.importClause.defaultName != null || importRecord.importClause.namedBindings.length > 0;
        const spec = toLocalModuleSpecifier(localPath, importRecord.moduleSpecifier);

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

                const symbols = `{${sortedStringBindings.join(", ")}}`;
                const bindings = [defaultName, symbols].filter(x => x != null).join(", ");
                ret.push(`import ${bindings} from "${spec}"`);
            }
        }

        return ret;
    }

    public formatImportBlock(localPath: string, importBlock: ImportBlock): string {
        const moduleSpecifiers = Object.keys(importBlock.importRecords);

        const sortedModuleSpecifiers = moduleSpecifiers.sort((a, b) => {
            const localA = toLocalModuleSpecifier(localPath, a);
            const localB = toLocalModuleSpecifier(localPath, b);

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
        }, [] as string[]).join(";\n");
    }
}
