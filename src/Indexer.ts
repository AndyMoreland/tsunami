import { RegionSpan } from "./protocol/types";

export interface Indexer {
    getDefinitionIndex(): DefinitionIndex;
}

export type DefinitionIndex = Map<string, Definition>;

export interface Definition {
    text: string | undefined;
    span: RegionSpan;
    moduleSpecifier: string;
    type: DefinitionType;
    default: boolean;
};

export enum DefinitionType {
    FUNCTION, EXPORTED_VAR, INTERFACE, ENUM, CLASS, TYPE, MODULE, EXTERNAL_MODULE
};
