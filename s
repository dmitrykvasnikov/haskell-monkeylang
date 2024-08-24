let map = {1 : "hello", true : 123, "key" : fn(x){x + 1}}

let func = map[12]

lookup("key", map)(100)
