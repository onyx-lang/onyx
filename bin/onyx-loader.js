
window.ONYX_MODULES  = window.ONYX_MODULES || [];
window.ONYX_MEMORY   = null;
window.ONYX_INSTANCE = null;

window.ONYX_MODULES.push({
    module_name: "host",

    print_str: function(ptr, len) {
        var buffer = new Uint8Array(ONYX_MEMORY.buffer, ptr, len);
        var string = new TextDecoder().decode(buffer);
        console.log(string);
    },

    exit: function() { debugger; }
});

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

window.onload = function() {
    var script_tags = document.getElementsByTagName("script");

    for (var i = 0; i < script_tags.length; i++) {
        if (script_tags[i].getAttribute("type") == "application/onyx") {
            // @ROBUSTNESS: It should be configurable which function is called on start up of a Onyx WASM module.
            launch_onyx_program(script_tags[i].getAttribute("src"), true);
        }
    }
};
