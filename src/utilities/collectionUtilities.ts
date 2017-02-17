export function compact<T>(collection: T[]): T[] {
    return collection.filter(x => x != null);
}
