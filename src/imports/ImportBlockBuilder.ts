import * as deepcopy from "deepcopy";
import * as ts from "typescript";
import { ImportBlock, ImportRecords } from "./ImportBlock";
import {
    ImportRecord,
    ModuleSpecifier,
    NamedBinding,
    createImportRecordFromImportDeclaration,
    getTypeOfModuleSpecifier,
    mergeImportRecords
} from "./ImportStatement";

export class ImportBlockBuilder {
    private built = false;

    public static from(block: ImportBlock) {
        const importRecords = deepcopy(block.importRecords);

        return new ImportBlockBuilder(importRecords);
    }

    public static fromFile(sourceFile: ts.SourceFile) {
        const importRecords: ImportRecords = {};

        ts.forEachChild(sourceFile, (node) => {
            if (node.kind === ts.SyntaxKind.ImportDeclaration) {
                const record = createImportRecordFromImportDeclaration(node as ts.ImportDeclaration);
                importRecords[record.moduleSpecifier] = mergeImportRecords(
                    record,
                    importRecords[record.moduleSpecifier]
                ).record;
            }
        });

        return new ImportBlockBuilder(importRecords);
    }

    public static empty() {
        return new ImportBlockBuilder();
    }

    constructor(private importRecords: ImportRecords = {}) {}

    public addSideEffectfulImport(moduleSpecifier: ModuleSpecifier): this {
        this.ensureImportRecord(moduleSpecifier);
        return this;
    }

    public addDefaultImport(moduleSpecifier: ModuleSpecifier, alias: string): this {
        this.ensureImportRecord(moduleSpecifier).importClause.defaultName = alias;
        return this;
    }

    public addNamespaceSpecifier(moduleSpecifier: ModuleSpecifier, alias: string): this {
        this.ensureImportRecord(moduleSpecifier).namespaceImport = { alias };
        return this;
    }

    /** Overwrites import alias if it already exists. */
    public addImportBinding(moduleSpecifier: ModuleSpecifier, namedBinding: NamedBinding): this {
        const importRecord = this.ensureImportRecord(moduleSpecifier);

        const clause = importRecord.importClause;
        if (namedBinding.symbolName === "default") {
            if (namedBinding.alias == null) {
                throw new Error("Named binding for default must specify name.");
            }

            clause.defaultName = namedBinding.alias;
        } else {
            const existingIndex = clause.namedBindings.findIndex(b => b.symbolName === namedBinding.symbolName);
            if (existingIndex >= 0) {
                clause.namedBindings[existingIndex] = namedBinding;
            } else {
                clause.namedBindings.push(namedBinding);
            }
        }

        return this;
    }

    public renameModule(from: ModuleSpecifier, to: ModuleSpecifier): this {
        this.importRecords[to] = this.importRecords[from];

        return this;
    }

    /** Specify `name === 'default' to remove default import`. Alias is ignored. */
    public withoutImport(moduleSpecifier: ModuleSpecifier, name: string): this {
        const importRecord = this.importRecords[moduleSpecifier];
        if (importRecord == null) {
            return this;
        }

        if (name === "default") {
            importRecord.importClause.defaultName = undefined;
        } else {
            const filteredBindings = importRecord.importClause.namedBindings.filter(x => x.symbolName !== name);
            importRecord.importClause.namedBindings = filteredBindings;
        }

        if (importRecord.importClause.namedBindings.length === 0
            && importRecord.importClause.defaultName == null
            && importRecord.namespaceImport == null
            && !importRecord.sideEffectful) {
            delete this.importRecords[moduleSpecifier];
        }

        return this;
    }

    public build(): ImportBlock {
        if (this.built) {
            throw new Error("Can't build ImportBlock twice.'");
        }

        this.built = true;

        return new ImportBlock(this.importRecords);
    }

    private ensureImportRecord(specifier: ModuleSpecifier): ImportRecord {
        if (this.importRecords[specifier] != null) {
            return this.importRecords[specifier];
        }

        const newSpecifier = {
            type: getTypeOfModuleSpecifier(specifier),
            moduleSpecifier: specifier,
            importClause: { namedBindings: []},
            sideEffectful: false
        };

        this.importRecords[specifier] = newSpecifier;

        return newSpecifier;
    }
}
