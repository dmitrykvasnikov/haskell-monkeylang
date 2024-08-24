let m = {1 : "hello", true : 123, "key" : fn(x){x + 10}}



let map = fn(arr, f) {
let iter = fn(arr, accumulated) {
if (len(arr) == 0) then {
accumulated
} else {
iter(rest(arr), push(accumulated, f(first(arr))));
}
};
iter(arr, []);
};

map([1,3,4], lookup("key", m))




