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
    return getTypeOfModuleSpecifier(declaration.getText());
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
                const name = specifier.name.getText();
                const propertyName = specifier.propertyName ? specifier.propertyName.getText() : undefined;

                if (name === "default" ) {
                    if (importClause.name != null) {
                        throw new Error("Invalid import specifier: found two default name bindings.");
                    }
                    parsedImportClause.defaultName = name;
                }  else {
                    parsedImportClause.namedBindings.push({
                        symbolName: name,
                        alias: propertyName
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

function canonicalizeModuleSpecifier(moduleSpecifier: string): AbsoluteFilename | ModuleName {
    const firstChar = moduleSpecifier.charAt(0);
    if (firstChar !== "." && firstChar !== "/") {
        return moduleSpecifier as ModuleName;
    } else {
        return path.resolve(moduleSpecifier) as AbsoluteFilename;
    }
}

function isNamespaceImport(parsed: NamespaceImport | ImportClause): parsed is NamespaceImport {
    return (parsed as NamespaceImport).alias != null;
}

export function createImportStatementFromImportDeclaration(declaration: ts.ImportDeclaration): ImportRecord {
    const record: ImportRecord = {
        type: getImportRecordTypeFromImportDeclaration(declaration),
        moduleSpecifier: canonicalizeModuleSpecifier(declaration.moduleSpecifier.getText()),
        importClause: {
            namedBindings: []
        }
    };

    if (declaration.importClause != null) {
        const parsed = parseImportClause(declaration.importClause);
        if (isNamespaceImport(parsed)) {
            record.namespaceImport = parsed;
        } else {
            record.importClause = parsed;
        }
    }

    return record;
}
