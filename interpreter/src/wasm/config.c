
#include "ovm_wasm.h"

wasm_config_t *wasm_config_new() {
    wasm_config_t *config = malloc(sizeof(*config));
    config->debug_enabled = false;
    config->listen_path   = "/tmp/ovm-debug.0000";
    return config;
}

void wasm_config_delete(wasm_config_t *config) {
    free(config);
}

void wasm_config_enable_debug(wasm_config_t *config, bool enabled) {
    config->debug_enabled = enabled;
}

void wasm_config_set_listen_path(wasm_config_t *config, char *listen_path) {
    config->listen_path = listen_path;
}

