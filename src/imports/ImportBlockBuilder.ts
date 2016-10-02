import { NamedBinding, getTypeOfModuleSpecifier, ModuleSpecifier } from "./ImportStatement";
import { ImportBlock, ImportRecords } from "./ImportBlock";
import * as deepcopy from "deepcopy";

export class ImportBlockBuilder {
    private built = false;
    constructor(private importRecords: ImportRecords = {}) {}

    public addSideEffectfulImport(moduleSpecifier: ModuleSpecifier): this {
        return this;
    }

    public addDefaultImport(moduleSpecifier: ModuleSpecifier, alias: string): this {
        return this;
    }

    /** Overwrites import alias if it already exists. */
    public addImportBinding(moduleSpecifier: ModuleSpecifier, namedBinding: NamedBinding): this {
        const importRecord = this.importRecords[moduleSpecifier] || {
            type: getTypeOfModuleSpecifier(moduleSpecifier),
            moduleSpecifier: moduleSpecifier,
            importClause: { namedBindings: []}
        };

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

        this.importRecords[moduleSpecifier] = importRecord;

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
            importRecord.importClause.namedBindings.filter(x => x.symbolName === name);
        }

        return this;
    }

    public static from(block: ImportBlock) {
        const importRecords = deepcopy(block.importRecords);

        return new ImportBlockBuilder(importRecords);
    }

    public static empty() {
        return new ImportBlockBuilder();
    }

    public build(): ImportBlock {
        if (this.built) {
            throw new Error("Can't build ImportBlock twice.'");
        }

        this.built = true;

        return new ImportBlock(this.importRecords);
    }
}
