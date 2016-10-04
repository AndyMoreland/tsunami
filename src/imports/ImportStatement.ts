import * as ts from "typescript";
import * as path from "path";

export enum ImportStatementType {
    PROJECT_RELATIVE, NODE, SCOPED_NODE
}

export interface NamedBinding {
    alias?: string;
    symbolName: string;
}

export interface ImportClause {
    defaultName?: string;
    namedBindings: NamedBinding[];
}

export interface NamespaceImport {
    alias: string;
}

export type ModuleSpecifier = AbsoluteFilename | ModuleName;
export type AbsoluteFilename = string & { __absoluteFilenameBrand: any };
export type ModuleName = string & { __moduleNameBrand: any };

export interface ImportRecord {
    type: ImportStatementType;
    moduleSpecifier: ModuleSpecifier;
    importClause: ImportClause;
    namespaceImport?: NamespaceImport;
    sideEffectful: boolean;
}

export function getTypeOfModuleSpecifier(moduleSpecifier: string): ImportStatementType {
    if (moduleSpecifier.charAt(0) === ".") {
        return ImportStatementType.PROJECT_RELATIVE;
    } else if (moduleSpecifier.charAt(0) === "@") {
        return ImportStatementType.SCOPED_NODE;
    } else {
        return ImportStatementType.NODE;
    }
}

function getImportRecordTypeFromImportDeclaration(declaration: ts.ImportDeclaration): ImportStatementType {
    return getTypeOfModuleSpecifier(declaration.moduleSpecifier.getText().slice(1, -1));
}

function parseImportClause(importClause: ts.ImportClause): ImportClause | NamespaceImport {
    const parsedImportClause: ImportClause = {
        namedBindings: []
    };

    if (importClause.name != null) {
        parsedImportClause.defaultName = importClause.name.getText();
    }

    if (importClause.namedBindings != null) {
        if (importClause.namedBindings.kind === ts.SyntaxKind.NamedImports) {
            const namedImports = importClause.namedBindings as ts.NamedImports;
            namedImports.elements.forEach(specifier => {
                let symbolName = specifier.name.getText();
                let alias: string | undefined = undefined;

                if (specifier.propertyName != null) {
                    alias = specifier.name.getText();
                    symbolName = specifier.propertyName.getText();
                }

                if (symbolName === "default" ) {
                    /*
                    if (importClause.name != null) {
                        throw new Error("Invalid import specifier: found two default name bindings.");
                    }
                    if (alias == null) {
                        throw new Error("Found default import in named bindings without alias.");
                    } */
                    parsedImportClause.defaultName = alias;
                }  else {
                    parsedImportClause.namedBindings.push({
                        symbolName,
                        alias
                    });
                }
            });
        } else if (importClause.namedBindings.kind === ts.SyntaxKind.NamespaceImport) {
            const namespaceBindings: NamespaceImport = {
                alias: (importClause.namedBindings as ts.NamespaceImport).name.getText()
            };

            return namespaceBindings;
        }
    }

    return parsedImportClause;
}

function canonicalizeModuleSpecifier(localDirectory: string, moduleSpecifier: string): AbsoluteFilename | ModuleName {
    const firstChar = moduleSpecifier.charAt(0);
    if (firstChar !== "." && firstChar !== "/") {
        return moduleSpecifier as ModuleName;
    } else {
        return path.resolve(localDirectory, moduleSpecifier).replace(/\.tsx?/g, "") as AbsoluteFilename;
    }
}

function isNamespaceImport(parsed: NamespaceImport | ImportClause): parsed is NamespaceImport {
    return (parsed as NamespaceImport).alias != null;
}

export function createImportRecordFromImportDeclaration(declaration: ts.ImportDeclaration): ImportRecord {
    const record: ImportRecord = {
        type: getImportRecordTypeFromImportDeclaration(declaration),
        moduleSpecifier: canonicalizeModuleSpecifier(
            path.dirname(declaration.getSourceFile().fileName),
            declaration.moduleSpecifier.getText().slice(1, -1)
        ),
        importClause: {
            namedBindings: []
        },
        sideEffectful: false
    };

    if (declaration.importClause != null) {
        const parsed = parseImportClause(declaration.importClause);
        if (isNamespaceImport(parsed)) {
            record.namespaceImport = parsed;
        } else {
            record.importClause = parsed;
        }
    } else {
        record.sideEffectful = true;
    }

    return record;
}

export interface Warning {
    message: string;
}

function mergeNamedBindings(a: NamedBinding[], b: NamedBinding[]): { namedBindings: NamedBinding[], warnings: Warning[] } {
    const warnings: Warning[] = [];
    const symbolsAlreadyNamed: { [symbol: string]: string | true } = { };
    const namedBindings = a.slice();

    namedBindings.forEach(binding => symbolsAlreadyNamed[binding.symbolName] = binding.alias || true);

    b.forEach(binding => {
        if (symbolsAlreadyNamed[binding.symbolName] != null) {
            warnings.push({
                message: `Import for ${binding.symbolName} aliased in multiple ways.`
            });
        } else {
            namedBindings.push(binding);
        }
    });

    return {
        namedBindings, warnings
    };
}

export function mergeImportRecords(a: ImportRecord, b?: ImportRecord): { record: ImportRecord, warnings: Warning[] } {
    if (b == null) {
        return { record: a, warnings: [] };
    }

    if (a.moduleSpecifier !== b.moduleSpecifier || a.type !== b.type) {
        throw new Error("Can't merge ImportRecords with non-identical module specifiers");
    }

    const warnings: Warning[] = [];
    const defaultName = a.importClause.defaultName || b.importClause.defaultName;
    const namespaceImport = a.namespaceImport || b.namespaceImport;
    const namedBindingsResult = mergeNamedBindings(a.importClause.namedBindings, b.importClause.namedBindings);
    const namedBindings = namedBindingsResult.namedBindings;
    warnings.push(...namedBindingsResult.warnings);

    if (a.importClause.defaultName && b.importClause.defaultName && a.importClause.defaultName !== b.importClause.defaultName) {
        warnings.push({
            // tslint:disable-next-line
            message: `Import for ${a.moduleSpecifier} specifies two default aliases: ${a.importClause.defaultName}, ${b.importClause.defaultName}`
        });
    }

    if (a.namespaceImport && b.namespaceImport && a.namespaceImport.alias !== b.namespaceImport.alias) {
        warnings.push({
            message: `Namespace imported in two conflicting ways for ${a.moduleSpecifier}`
        });
    }

    const newRecord: ImportRecord = {
        type: a.type, /* equal to b's type */
        moduleSpecifier: a.moduleSpecifier, /* equal to b's specifier */
        importClause: {
            defaultName,
            namedBindings
        },
        namespaceImport,
        sideEffectful: a.sideEffectful || b.sideEffectful
    };

    return {
        record: newRecord,
        warnings
    };
}
