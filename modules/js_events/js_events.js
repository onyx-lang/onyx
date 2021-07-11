window.ONYX_MODULES = window.ONYX_MODULES || [];

function push_event_to_buffer(esp, event_size, event_kind, data) {
    let WASM_U32 = new Uint32Array(ONYX_MEMORY.buffer);

    if (WASM_U32[esp] >= WASM_U32[esp + 1]) {
        console.log("Event buffer full!");
        return;
    }

    WASM_U32[esp] += 1;

    let event_idx = esp + (WASM_U32[esp] - 1) * (event_size / 4) + 2;
    WASM_U32[event_idx] = event_kind;
    WASM_U32[event_idx + 1] = Date.now();

    for (let i = 0; i < data.length; i++) {
        WASM_U32[event_idx + 2 + i] = data[i];
    }
}

var requested_file_data = {};

window.ONYX_MODULES.push({
    module_name: "js_events",

    setup: function(esp, event_size) {
        // Indicies into a Uint32Array are not based on bytes,
        // but on the index.
        esp /= 4;

        document.addEventListener("keydown", function (ev) {
            if (ev.isComposing || ev.keyCode === 229) return;
            ev.preventDefault();

            // NOTE: These modifiers need to match in js_events.onyx.
            var modifiers = 0x0000;
            if (ev.ctrlKey)  modifiers |= 0x01;
            if (ev.altKey)   modifiers |= 0x02;
            if (ev.metaKey)  modifiers |= 0x04;
            if (ev.shiftKey) modifiers |= 0x08;

            push_event_to_buffer(esp, event_size, 0x04, [ ev.keyCode, modifiers ]);

            var keyname = ev.code;
            let WASM_U32 = new Uint32Array(ONYX_MEMORY.buffer);
            let event_idx = esp + (WASM_U32[esp] - 1) * (event_size / 4) + 2;

            let WASM_U8 = new Uint8Array(ONYX_MEMORY.buffer);

            for (var i = 0; i < keyname.length; i++) {
                WASM_U8[event_idx * 4 + (4 * 4) + i] = keyname.charCodeAt(i);
            }

            WASM_U8[event_idx * 4 + (4 * 4) + 15] = keyname.length;
            return false;
        });

        document.addEventListener("keyup", function (ev) {
            if (ev.isComposing || ev.keyCode === 229) return;
            ev.preventDefault();

            // NOTE: These modifiers need to match in js_events.onyx.
            var modifiers = 0x0000;
            if (ev.ctrlKey)  modifiers |= 0x01;
            if (ev.altKey)   modifiers |= 0x02;
            if (ev.metaKey)  modifiers |= 0x04;
            if (ev.shiftKey) modifiers |= 0x08;
            
            push_event_to_buffer(esp, event_size, 0x05, [ ev.keyCode, modifiers ]);

            var keyname = ev.code;
            let WASM_U32 = new Uint32Array(ONYX_MEMORY.buffer);
            let event_idx = esp + (WASM_U32[esp] - 1) * (event_size / 4) + 2;

            let WASM_U8 = new Uint8Array(ONYX_MEMORY.buffer);

            for (var i = 0; i < keyname.length; i++) {
                WASM_U8[event_idx * 4 + (4 * 4) + i] = keyname.charCodeAt(i);
            }

            WASM_U8[event_idx * 4 + (4 * 4) + 15] = keyname.length;

            return false;
        });

        document.addEventListener("mousedown", function (ev) {
            push_event_to_buffer(esp, event_size, 0x01, [ ev.clientX, ev.clientY, ev.button ]);
        });

        document.addEventListener("mouseup", function (ev) {
            push_event_to_buffer(esp, event_size, 0x02, [ ev.clientX, ev.clientY, ev.button ]);
        });

        document.addEventListener("mousemove", function (ev) {
            push_event_to_buffer(esp, event_size, 0x03, [ ev.clientX, ev.clientY, -1 ]);
        });

        document.addEventListener("wheel", function (ev) {
            push_event_to_buffer(esp, event_size, 0x07, [ ev.clientX, ev.clientY, ev.deltaY >= 0 ? 0x04 : 0x03 ]);
        });

        window.addEventListener("resize", function (ev) {
            push_event_to_buffer(esp, event_size, 0x06, [ window.innerWidth, window.innerHeight ]);
        });

        push_event_to_buffer(esp, event_size, 0x06, [ window.innerWidth, window.innerHeight ]);

        document.oncontextmenu = (e) => {
            e.preventDefault = true;
            return false;
        };
    },

    request_file(esp, event_size, filename_ptr, filename_len, fileid) {
        esp /= 4;

        var path_memory = new Uint8Array(ONYX_MEMORY.buffer, filename_ptr, filename_len);
        var path = new TextDecoder("utf-8").decode(path_memory);
        console.log(path);

        fetch(path)
            .then(response => response.arrayBuffer())
            .then(array_buffer => {
                requested_file_data[fileid] = array_buffer;

                push_event_to_buffer(esp, event_size, 0x09, [ 0x01, fileid, array_buffer.byteLength ]);
            })
            .catch((error) => {
                push_event_to_buffer(esp, event_size, 0x09, [ 0x02, fileid, 0 ]);
            });
    },

    get_requested_file_data(fileid, bufferptr, bufferlen) {
        var file_data = requested_file_data[fileid];
        if (file_data == null) return 0;

        if (bufferlen < file_data.byteLength) return 0;

        let WASM_U8 = new Uint8Array(ONYX_MEMORY.buffer);
        var u8_data = new Uint8Array(file_data);

        WASM_U8.set(u8_data, bufferptr);

        requested_file_data[fileid] = null;
        delete requested_file_data[fileid];

        return 1;
    },
});

