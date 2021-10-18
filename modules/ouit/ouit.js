window.ONYX_MODULES = window.ONYX_MODULES || [];

window.ONYX_MODULES.push({
    module_name: "ouit",

    loop: function() {
        function loop() {
            window.ONYX_INSTANCE.exports.ouit_loop();
            window.requestAnimationFrame(loop);
        }

        window.requestAnimationFrame(loop);
    },

    time_now: function() {
        return Date.now();
    },
});