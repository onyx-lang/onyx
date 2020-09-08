WebGl_Wasm = {
    init(name, namelen) {
        const decoder = new TextDecoder();
        const str = new Uint8Array(WASM_MEMORY.buffer, name, namelen);
        const canvasname = decoder.decode(str);

        this.programs = [];
        this.shaders = [];
        this.buffers = [];
        this.framebuffers = [];
        this.renderbuffers = [];
        this.textures = [];
        this.uniformlocs = [];
        this.vertexArrays = [];

        this.canvas = document.getElementById(canvasname);
        if (this.canvas == null) return 0;

        this.gl = this.canvas.getContext("webgl2");
        if (this.gl == null) return 0;

        return 1;
    },

    activeTexture(texture) { this.gl.activeTexture(texture); },
    attachShader(program, shader) { this.gl.attachShader(this.programs[program], this.shaders[shader]); return this.programs[program]; },
    bindAttribLocation(program, index, name, namelen) { console.log("NOT IMPLEMENTED!"); },
    bindBuffer(target, buffer) {
        if (buffer == -1) {
            this.gl.bindBuffer(target, null);
        } else {
            this.gl.bindBuffer(target, this.buffers[buffer]);
        }
    },
    bindFramebuffer(target, framebuffer) { this.gl.bindFramebuffer(target, this.framebuffers[framebuffer]); },
    bindRenderbuffer(target, renderbuffer) { this.gl.bindRenderbuffer(target, this.renderbuffers[renderbuffer]); },
    bindTexture(target, texture) { this.gl.bindTexture(target, this.textures[texture]); },
    bindVertexArray(vertexArray) { this.gl.bindVertexArray(this.vertexArrays[vertexArray]); },

    blendColor(red, green, blue, alpha) { this.gl.blendColor(red, green, blue, alpha); },
    blendEquation(mode) { this.gl.blendEquation(mode); },
    blendEquationSeparate(modeRGB, modeAlpha) { this.gl.blendEquationSeparate(modeRGB, modeAlpha); },
    blendFunc(sfactor, dfactor) { this.gl.blendFunc(sfactor, dfactor); },
    blendFuncSeparate(srcRGB, dstRGB, srcAlpha, dstAlpha) { this.gl.blendFuncSeparate(srcRGB, dstRGB, srcAlpha, dstAlpha); },

    blitFramebuffer(sx0, sy0, sx1, sy1, dx0, dy0, dx1, dy1, mask, filter) {
        this.gl.blitFramebuffer(sx0, sy0, sx1, sy1, dx0, dy0, dx1, dy1, mask, filter);
    },

    bufferDataWithData(target, bufferdata, bufferlen, usage) {
        const data = new DataView(WASM_MEMORY.buffer, bufferdata, bufferlen);
        this.gl.bufferData(target, data, usage);
    },

    bufferDataNoData(target, size, usage) { this.gl.bufferData(target, size, usage); },
    bufferSubData(target, offset, bufferdata, bufferlen) {
        const data = new DataView(WASM_MEMORY.buffer, bufferdata, bufferlen);
        this.gl.bufferSubData(target, offset, data);
    },
    canvasSize(width, height) {
        this.canvas.width = width;
        this.canvas.height = height;
    },
    checkFrameBufferStatus(target) { return this.gl.checkFrameBufferStatus(target); },
    clear(bit) { this.gl.clear(bit); },
    clearColor(r, g, b, a) { this.gl.clearColor(r, g, b, a); },
    clearDepth(depth) { this.gl.clearDepth(depth); },
    clearStencil(stencil) { this.gl.clearStencil(stencil); },
    colorMask(r, g, b, a) { this.gl.colorMask(r, g, b, a); },
    compileShader(shader) { this.gl.compileShader(this.shaders[shader]); },
    compressedTexImage2D(target, level, internalformat, width, height, border, data, datalen) {
        const pixels = new DataView(WASM_MEMORY.buffer, data, datalen);
        this.gl.compressedTexImage2D(target, level, internalformat, width, height, border, pixels);
    },
    compressedTexSubImage2D(target, level, internalformat, xoff, yoff, width, height, format, data, datalen) {
        const pixels = new DataView(WASM_MEMORY.buffer, data, datalen);
        this.gl.compressedSubTexImage2D(target, level, internalformat, xoff, yoff, width, height, format, pixels);
    },
    copyBufferSubData(readTarget, writeTarget, readOffset, writeOffset, size) { this.gl.copyBufferSubData(readTarget, writeTarget, readOffset, writeOffset, size); },
    copyTexImage2D(target, level, internalforamt, x, y, width, height, border) {
        this.gl.copyTexImage2D(target, level, internalforamt, x, y, width, height, border);
    },
    copyTexSubImage2D(target, level, xoff, yoff, x, y, width, height) {
        this.gl.copyTexSubImage2D(target, level, xoff, yoff, x, y, width, height);
    },
    createBuffer() {
        const buf = this.gl.createBuffer();
        if (buf == null) return -1;

        this.buffers.push(buf);
        return this.buffers.length - 1;
    },
    createFramebuffer() {
        const buf = this.gl.createFramebuffer();
        if (buf == null) return -1;

        this.framebuffers.push(buf);
        return this.framebuffers.length - 1;
    },
    createProgram() {
        const prog = this.gl.createProgram();
        if (prog == null) return -1;

        this.programs.push(prog);
        return this.programs.length - 1;
    },
    createRenderbuffer() {
        const buf = this.gl.createRenderbuffer();
        if (buf == null) return -1;

        this.renderbuffers.push(buf);
        return this.renderbuffers.length - 1;
    },
    createShader(type) {
        const shader = this.gl.createShader(type);
        if (shader == null) return -1;

        this.shaders.push(shader);
        return this.shaders.length - 1;
    },
    createTexture() {
        const texture = this.gl.createTexture();
        if (texture == null) return -1;

        this.textures.push(texture);
        return this.textures.length - 1;
    },
    createVertexArray() {
        const vao = this.gl.createVertexArray();
        if (vao == null) return -1;

        this.vertexArrays.push(vao);
        return this.vertexArrays.length - 1;
    },
    cullFace(mode) { this.gl.cullFace(mode); },
    deleteBuffer(buffer) { this.gl.deleteBuffer(this.buffers[buffer]); },
    deleteFramebuffer(framebuffer) { this.gl.deleteFramebuffer(this.framebuffers[framebuffer]); },
    deleteProgram(program) { this.gl.deleteProgram(this.programs[program]); },
    deleteRenderbuffer(renderbuffer) { this.gl.deleteRenderbuffer(this.renderbuffers[renderbuffer]); },
    deleteShader(shader) { this.gl.deleteShader(this.shaders[shader]); },
    deleteTexture(texture) { this.gl.deleteTexture(this.textures[texture]); },
    deleteVertexArray(vertexArray) { this.gl.deleteVertexArray(this.vertexArrays[vertexArray]); },
    depthFunc(func) { this.gl.depthFunc(func); },
    depthMask(flag) { this.gl.depthMask(flag); },
    depthRange(znear, zfar) { this.gl.depthRange(znear, zfar); },
    detachShader(program, shader) { this.gl.detachShader(this.programs[program], this.shaders[shader]); },
    disable(cap) { this.gl.disable(cap); },
    disableVertexAttribArray(index) { this.gl.disableVertexAttribArray(index); },
    drawArrays(mode, first, count) { this.gl.drawArrays(mode, first, count); },
    drawArraysInstanced(mode, first, count, instanceCount) { this.gl.drawArraysInstanced(mode, first, count, instanceCount); },
    drawElements(mode, count, type, offset) { this.gl.drawElements(mode, count, type, offset); },
    drawElementsInstanced(mode, count, type, offset, instanceCount) { this.gl.drawElementsInstanced(mode, count, type, offset, instanceCount); },
    enable(cap) { this.gl.enable(cap); },
    enableVertexAttribArray(index) { this.gl.enableVertexAttribArray(index); },
    finish() { this.gl.finish(); },
    flush() { this.gl.flush(); },
    framebufferRenderbuffer(target, attachment, renderbuffertarget, renderbuffer) {
        this.gl.framebufferRenderbuffer(target, attachment, renderbuffertarget, this.renderbuffers[renderbuffer]);
    },
    framebufferTexture2D(target, attachment, texttarget, texture, level) {
        this.gl.framebufferTexture2D(target, attachment, texttarget, this.textures[texture], level);
    },
    framebufferTextureLayer(target, attachment, texture, level, layer) { this.gl.framebufferTextureLayer(target, attachment, this.textures[texture], level, layer); },
    frontFace(mode) { this.gl.frontFace(mode); },
    generateMipmap(target) { this.gl.generateMipmap(target); },
    getActiveAttrib(program, index, out) {
        const loc = this.gl.getActiveAttrib(this.programs[program], index);
        const data = new Int32Array(WASM_MEMORY.buffer, out, 2);
        data[0] = loc.size;
        data[1] = loc.type;
    },
    getActiveUniform(program, index, out) {
        const loc = this.gl.getActiveUniform(this.programs[program], index);
        const data = new Int32Array(WASM_MEMORY.buffer, out, 2);
        data[0] = loc.size;
        data[1] = loc.type;
    },
    // getAttachedShaders() { console.log("NOT IMPLEMENTED!"); },
    getAttribLocation(program, name, namelen) {
        const decoder = new TextDecoder();
        const str = new Uint8Array(WASM_MEMORY.buffer, name, namelen);
        const attribname = decoder.decode(str);

        return this.gl.getAttribLocation(this.programs[program], attribname);
    },
    // getBufferParameter() { console.log("NOT IMPLEMENTED!"); },
    getBufferSubData(target, srcbyteoffset, dstbufferdata, dstbufferlen, dstoffset, length) {
        const dst = new DataView(WASM_MEMORY.buffer, dstbufferdata, dstbufferlen);
        this.gl.getBufferSubData(target, srcbyteoffset, dst, dstoffset, length);
    },
    getError() { return this.gl.getError(); },
    getInternalformatParameter(target, internalformat, pname) { return this.gl.getInternalformatParameter(target, internalformat, pname); },
    // many of the 'gets() { console.log("NOT IMPLEMENTED!"); },
    getShaderParameter(shader, param) { return this.gl.getShaderParameter(this.shaders[shader], param); },
    getProgramParameter(program, param) { return this.gl.getProgramParameter(this.programs[program], param); },
    getUniformLocation(program, name, namelen) {
        const decoder = new TextDecoder();
        const str = new Int8Array(WASM_MEMORY.buffer, name, namelen);
        const uniname = decoder.decode(str);

        this.uniformlocs.push(this.gl.getUniformLocation(this.programs[program], uniname));
        return this.uniformlocs.length - 1;
    },
    getVertexAttribOffset(index, pname) { return this.gl.getVertexAttribOffset(index, pname); },
    hint(target, mode) { this.gl.hint(target, mode); },
    isEnabled(cap) { return this.gl.isEnabled(cap); },
    invalidateFramebuffer(target, attachdata, attachlen) {
        const attachments = new Int32Array(WASM_MEMORY.buffer, attachdata, attachlen);
        this.gl.invalidateFramebuffer(target, attacements);
    },
    invalidateSubFramebuffer(target, attachdata, attachlen, x, y, width, height) {
        const attachments = new Int32Array(WASM_MEMORY.buffer, attachdata, attachlen);
        this.gl.invalidateFramebuffer(target, attacements, x, y, width, height);
    },
    lineWidth(width) { this.gl.lineWidth(width); },
    linkProgram(program) { this.gl.linkProgram(this.programs[program]); },
    pixelStorei(pname, param) { this.gl.pixelStorei(pname, param); },
    polygonOffset(factor, units) { this.gl.polygonOffset(factor, units); },
    printProgramInfoLog(program) { console.log(this.gl.getProgramInfoLog(this.programs[program])); },
    printShaderInfoLog(shader) { console.log(this.gl.getShaderInfoLog(this.shaders[shader])); },
    readPixels(x, y, width, height, format, type, pixels, pixelslen) {
        const pixeldata = new Uint8Array(WASM_MEMORY.buffer, pixels, pixelslen);
        this.gl.readPixels(x, y, width, height, format, type, pixeldata);
    },
    readBuffer(src) { this.gl.readBuffer(src); },
    renderbufferStorageMultisample(target, samples, internalforamt, width, height) {
        this.gl.renderbufferStorageMultisample(target, samples, internalforamt, width, height);
    },
    sampleCoverage(value, invert) { this.gl.sampleCoverage(value, invert); },
    scissor(x, y, width, height) { this.gl.scissor(x, y, width, height); },
    shaderSource(shader, source, sourcelen) {
        const decoder = new TextDecoder();
        const str = new Int8Array(WASM_MEMORY.buffer, source, sourcelen);
        const sourcedata = decoder.decode(str);

        this.gl.shaderSource(this.shaders[shader], sourcedata);
    },
    stencilFunc(func, ref, mask) { this.gl.stencilFunc(func, ref, mask); },
    stencilFuncSeparate(face, func, ref, mask) { this.gl.stencilFuncSeparate(face, func, ref, mask); },
    stencilMask(mask) { this.gl.stencilMask(mask); },
    stencilMaskSeparate(face, mask) { this.gl.stencilMaskSeparate(face, mask); },
    stencilOp(fail, zfail, mask) { this.gl.stencilOp(fail, zfail, mask); },
    stencilOpSeparate(face, fail, zfail, zpass) { this.gl.stencilOpSeparate(face, fail, zfail, zpass); },
    texImage2D(target, level, internalforamt, width, height, border, format, type, pixels, pixelslen) {
        const data = new DataView(WASM_MEMORY.buffer, pixels, pixelslen);
        this.gl.texImage2D(target, level, internalforamt, width, height, border, format, type, data);
    },
    texParameterf(target, pname, param) { this.gl.texParameterf(target, pname, param); },
    texParameteri(target, pname, param) { this.gl.texParameteri(target, pname, param); },
    texSubImage2D(target, level, xoff, yoff, width, height, format, type, pixels, pixelslen) {
        const data = new Uint8Array(WASM_MEMORY.buffer, pixels, pixelslen);
        this.gl.texSubImage2D(target, level, xoff, yoff, width, height, format, type, data);
    },
    uniform1f(loc, x) { this.gl.uniform1f(this.uniformlocs[loc], x); },
    uniform1i(loc, x) { this.gl.uniform1i(this.uniformlocs[loc], x); },
    uniform2f(loc, x, y) { this.gl.uniform2f(this.uniformlocs[loc], x, y); },
    uniform2i(loc, x, y) { this.gl.uniform2i(this.uniformlocs[loc], x, y); },
    uniform3f(loc, x, y, z) { this.gl.uniform3f(this.uniformlocs[loc], x, y, z); },
    uniform3i(loc, x, y, z) { this.gl.uniform3i(this.uniformlocs[loc], x, y, z); },
    uniform4f(loc, x, y, z, w) { this.gl.uniform4f(this.uniformlocs[loc], x, y, z, w); },
    uniform4i(loc, x, y, z, w) { this.gl.uniform4i(this.uniformlocs[loc], x, y, z, w); },
    uniformMatrix2(loc, transpose, valueptr) {
        const data = new Float32Array(WASM_MEMORY.buffer, valueptr, 4);
        this.gl.uniformMatrix2fv(this.uniformlocs[loc], transpose, data);
    },
    uniformMatrix3(loc, transpose, valueptr) {
        const data = new Float32Array(WASM_MEMORY.buffer, valueptr, 9);
        this.gl.uniformMatrix3fv(this.uniformlocs[loc], transpose, data);
    },
    uniformMatrix4(loc, transpose, valueptr) {
        const data = new Float32Array(WASM_MEMORY.buffer, valueptr, 16);
        this.gl.uniformMatrix4fv(this.uniformlocs[loc], transpose, data);
    },
    useProgram(program) { this.gl.useProgram(this.programs[program]); },
    validateProgram(program) { this.gl.validateProgram(this.program[program]); },
    vertexAttrib1f(idx, x) { this.gl.vertexAttrib1f(idx, x); },
    vertexAttrib2f(idx, x, y) { this.gl.vertexAttrib2f(idx, x, y); },
    vertexAttrib3f(idx, x, y, z) { this.gl.vertexAttrib3f(idx, x, y, z); },
    vertexAttrib4f(idx, x, y, z, w) { this.gl.vertexAttrib4f(idx, x, y, z, w); },
    vertexAttribPointer(idx, size, type, normalized, stride, offset) { this.gl.vertexAttribPointer(idx, size, type, normalized, stride, offset); },
    vertexAttribDivisor(idx, divisor) { this.gl.vertexAttribDivisor(idx, divisor); },
    viewport(x, y, width, height) { this.gl.viewport(x, y, width, height); },

};
