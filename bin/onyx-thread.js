
function onyx_decode_text(ptr, len) {
    let v = new DataView(self.ONYX_MEMORY.buffer);

    let s = "";
    for (let i = 0; i < len; i++) {
        s += String.fromCharCode(v.getUint8(ptr + i));
    }

    return s;
}


self.onmessage = function (msg) {
    const data = msg.data;

    let import_object = {};
    for (let k of Object.keys(data.imports)) {
        import_object[k] = {};
        for (let v of data.imports[k]) {
            import_object[k][v] = function(a, b, c, d, e, f, g, h, i, j) {
                console.error("ATTEMPT TO CALL MAIN THREAD FUNCTION FROM WORKER THREAD! " + v + "." + k);
            }
        }
    }

    import_object.host.print_str = function(ptr, len) { console.log(onyx_decode_text(ptr, len)); };
    import_object.host.exit      = function() { debugger; };
    import_object.onyx           = { memory: data.memory };

    WebAssembly.instantiate(new Uint8Array(data.wasm_bytes), import_object)
    .then(function(res) {
        self.ONYX_MEMORY = data.memory;

        res.instance.exports._thread_start(data.thread_id, data.tls_base, data.stack_base, data.funcidx, data.dataptr);
        res.instance.exports._thread_exit(data.thread_id);
    });
,
