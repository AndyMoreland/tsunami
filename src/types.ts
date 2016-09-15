export type Map<V> = { [index: string]: V };

export interface CallbackFunction<T> {
    (error?: Error | null, data?: T): void;
}

export type UnknownObject = Map<any>;
