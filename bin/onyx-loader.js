
window.ONYX_MODULES  = window.ONYX_MODULES || [];
window.ONYX_MEMORY   = null;
window.ONYX_INSTANCE = null;
window.ONYX_BYTES    = null;
window.ONYX_THREAD_SCRIPT = "onyx-thread.js";
window.ONYX_WORKERS  = {};

window.ONYX_MODULES.push({
    module_name: "host",

    print_str: function(ptr, len) {
        console.log(onyx_decode_text(ptr, len));
    },

    exit: function() { debugger; },

    spawn_thread: function(id, tls_base, stack_base, funcidx, dataptr) {
        try {
            let needed_imports = {};

            for (let i = 0; i < window.ONYX_MODULES.length; i++) {
                needed_imports[window.ONYX_MODULES[i].module_name] = [];

                for (let k of Object.keys(window.ONYX_MODULES[i])) {
                    if (k == "module_name") continue;

                    needed_imports[window.ONYX_MODULES[i].module_name].push(k);
                }
            }

            window.ONYX_WORKERS[id] = new Worker(window.ONYX_THREAD_SCRIPT);
            window.ONYX_WORKERS[id].postMessage({
                thread_id  : id,
                memory     : window.ONYX_MEMORY,
                wasm_bytes : window.ONYX_BYTES,
                funcidx    : funcidx,
                dataptr    : dataptr,
                imports    : needed_imports,
                tls_base   : tls_base,
                stack_base : stack_base
            });

            return 1;

        } catch (e) {
            console.error(e);
            return 0;
        }
    },

    kill_thread(id) {
        if (window.ONYX_WORKERS[id] == null) return 0;

        window.ONYX_WORKERS[id].terminate();
        delete window.ONYX_WORKERS[id];
        ONYX_WORKERS[id] = null;

        return 1;
    },
});

function onyx_decode_text(ptr, len) {
    let v = new DataView(window.ONYX_MEMORY.buffer);

    let s = "";
    for (let i = 0; i < len; i++) {
        s += String.fromCharCode(v.getUint8(ptr + i));
    }

    return s;
}

function launch_onyx_program(script_path, call_start) {
    fetch(script_path)
    .then(function(res) { return res.arrayBuffer(); })
    .then(function(wasm_code) {
        var import_object = {};

        for (var i = 0; i < window.ONYX_MODULES.length; i++) {
            import_object[window.ONYX_MODULES[i].module_name] = window.ONYX_MODULES[i];
        }

        return WebAssembly.instantiate(wasm_code, import_object);
    })
    .then(function(wasm_module) {
        window.ONYX_MEMORY = wasm_module.instance.exports.memory;
        window.ONYX_INSTANCE = wasm_module.instance;

        wasm_module.instance.exports._start();
    });
}

function launch_multi_threaded_onyx_program(script_path, call_start) {
    fetch(script_path)
    .then(function(res) { return res.arrayBuffer(); })
    .then(function(data) {
        var import_object = {};

        for (var i = 0; i < window.ONYX_MODULES.length; i++) {
            import_object[window.ONYX_MODULES[i].module_name] = window.ONYX_MODULES[i];
        }

        import_object["onyx"] = { memory: new WebAssembly.Memory({ initial: 1024, maximum: 65536, shared: true }) };
        window.ONYX_MEMORY = import_object["onyx"]["memory"];
        window.ONYX_BYTES  = data;

        WebAssembly.instantiate(data, import_object)
        .then(function (code_module) {
            window.ONYX_INSTANCE = code_module.instance;
            code_module.instance.exports._start();
        });
    });
}

window.onload = function() {
    var script_tags = document.getElementsByTagName("script");

    for (var i = 0; i < script_tags.length; i++) {
        if (script_tags[i].getAttribute("type") == "application/onyx") {
            if (script_tags[i].getAttribute("multi-threaded")) {
                launch_multi_threaded_onyx_program(script_tags[i].getAttribute("src"), true);
            } else {
                launch_onyx_program(script_tags[i].getAttribute("src"), true);
            }
        }
    }
};
