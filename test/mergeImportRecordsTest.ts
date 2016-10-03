import { mergeImportRecords, ModuleSpecifier, ImportStatementType, ImportRecord } from "../src/imports/ImportStatement";
import { expect } from "chai";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

const MODULE_1 = "1" as ModuleSpecifier;

const SYMBOL_1_IMPORT: ImportRecord = {
    type: ImportStatementType.NODE,
    moduleSpecifier: MODULE_1,
    importClause: {
        namedBindings: [{symbolName: "symbol-1"}]
    }
};

const SYMBOL_2_IMPORT: ImportRecord = {
    type: ImportStatementType.NODE,
    moduleSpecifier: MODULE_1,
    importClause: {
        namedBindings: [{symbolName: "symbol-2"}]
    }
};

const DEFAULT_ALIASED_IMPORT: ImportRecord = {
    type: ImportStatementType.NODE,
    moduleSpecifier: MODULE_1,
    importClause: {
        namedBindings: [],
        defaultName: "default-alias"
    }
};

const NAMESPACE_BINDING_IMPORT: ImportRecord = {
    type: ImportStatementType.NODE,
    moduleSpecifier: MODULE_1,
    importClause: {
        namedBindings: [],
    },
    namespaceImport: {
        alias: "namespace-alias"
    }
};

describe("mergeImportRecords", () => {
    describe("when merging import records that both specify named bindings", () => {

        const merged = mergeImportRecords(SYMBOL_1_IMPORT, SYMBOL_2_IMPORT);

        it ("should accumulate different symbols", () => {
            const bindings = merged.record.importClause.namedBindings;
            expect(bindings).to.contain({ symbolName: "symbol-1" });
            expect(bindings).to.contain({ symbolName: "symbol-2" });
        });
    });

    describe("when merging an aliased symbol and a default alias", () => {
        const merged = mergeImportRecords(SYMBOL_1_IMPORT, DEFAULT_ALIASED_IMPORT);

        it ("should contain the aliased symbol and the default alias", () => {
            const bindings = merged.record.importClause.namedBindings;
            expect(bindings).to.contain({ symbolName: "symbol-1" });
            expect(bindings.length).to.eql(1);
            expect(merged.record.importClause.defaultName).to.eql("default-alias");
        });
    });

    describe("when merging an aliased namespace and a symbol import", () => {
        const merged = mergeImportRecords(SYMBOL_1_IMPORT, NAMESPACE_BINDING_IMPORT);

        it ("should contain the aliased namespace and the symbol import", () => {
            const bindings = merged.record.importClause.namedBindings;
            expect(bindings).to.contain({ symbolName: "symbol-1" });
            expect(bindings.length).to.eql(1);
            expect(merged.record.namespaceImport).to.not.be.null;
            expect(merged.record.namespaceImport!.alias).to.eql("namespace-alias");
        });
    });
});
