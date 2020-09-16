const fs = require('fs');
const buf = fs.readFileSync(process.argv[2]);

let wasm_memory;

const ENV = {
    host: {
        print_str(ptr, len) {
            const data = new Uint8Array(wasm_memory, ptr, len);
            const str  = new TextDecoder().decode(data);
            console.log(str);
        }
    }   
}

WebAssembly.instantiate(new Uint8Array(buf), ENV)
    .then(res => {
        const lib = res.instance.exports;
        wasm_memory = lib.memory.buffer;

        lib._start();
    });

