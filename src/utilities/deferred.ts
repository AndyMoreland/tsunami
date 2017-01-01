export class DeferredResponse<T> {
    static empty<T>(): DeferredResponse<T> {
        return new DeferredResponse<T>();
    }

    static of<T>(value: T): DeferredResponse<T> {
        const deferred = new DeferredResponse<T>();
        deferred.resolve(value);
        return deferred;
    }

    constructor() {
        this.promise = new Promise<T>((resolve, reject) => {
            this.resolve = resolve as any;
            this.reject = reject as any;
        });
    }

    promise: Promise<T>;
    resolve: (value: T) => Promise<T>;
    reject: (error?: Error) => void;
}
