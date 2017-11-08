export function identity(value) {
    return value;
}

export var encodeNull = null;

export function encodeObject(values) {
    var obj = {};
    while (values.head !== undefined) {
        var pair = values.head;
        obj[pair[0]] = pair[1];
        values = values.tail;
    }
    return obj;
}

export var stringify = JSON.stringify;

export function encodeList(values) {
    var array = [];
    while (values.head !== undefined) {
        array = array.concat(values.head)
        values = values.tail;
    }
    return array;

}
