window.ONYX_MODULES = window.ONYX_MODULES || [];

var programs = [];
var shaders = [];
var buffers = [];
var framebuffers = [];
var renderbuffers = [];
var textures = [];
var uniformlocs = [];
var vertexArrays = [];
var canvas = null;
var gl = null;

window.ONYX_MODULES.push({
    module_name: "gl",

    init(name, namelen) {
        const canvasname = onyx_decode_text(name, namelen);

        canvas = document.getElementById(canvasname);
        if (canvas == null) return 0;

        gl = canvas.getContext("webgl2");
        if (gl == null) return 0;

        return 1;
    },

    activeTexture(texture) { gl.activeTexture(texture); },
    attachShader(program, shader) { gl.attachShader(programs[program], shaders[shader]); return programs[program]; },
    bindAttribLocation(program, index, name, namelen) { console.log("NOT IMPLEMENTED!"); },
    bindBuffer(target, buffer) {
        if (buffer == -1) {
            gl.bindBuffer(target, null);
        } else {
            gl.bindBuffer(target, buffers[buffer]);
        }
    },
    bindFramebuffer(target, framebuffer) {
        if (framebuffer == -1) {
            gl.bindFramebuffer(target, null);
        } else {
            gl.bindFramebuffer(target, framebuffers[framebuffer]);
        }
    },
    bindRenderbuffer(target, renderbuffer) {
        if (renderbuffer == -1) {
            gl.bindRenderbuffer(target, null);
        } else {
            gl.bindRenderbuffer(target, renderbuffers[renderbuffer]);
        }
    },
    bindTexture(target, texture) {
        if (texture == -1) {
            gl.bindTexture(target, null);
        } else {
            gl.bindTexture(target, textures[texture]);
        }
    },
    bindVertexArray(vertexArray) {
        if (vertexArray == -1) {
            gl.bindVertexArray(null);
        } else {
            gl.bindVertexArray(vertexArrays[vertexArray]); 
        }
    },

    blendColor(red, green, blue, alpha) { gl.blendColor(red, green, blue, alpha); },
    blendEquation(mode) { gl.blendEquation(mode); },
    blendEquationSeparate(modeRGB, modeAlpha) { gl.blendEquationSeparate(modeRGB, modeAlpha); },
    blendFunc(sfactor, dfactor) { gl.blendFunc(sfactor, dfactor); },
    blendFuncSeparate(srcRGB, dstRGB, srcAlpha, dstAlpha) { gl.blendFuncSeparate(srcRGB, dstRGB, srcAlpha, dstAlpha); },

    blitFramebuffer(sx0, sy0, sx1, sy1, dx0, dy0, dx1, dy1, mask, filter) {
        gl.blitFramebuffer(sx0, sy0, sx1, sy1, dx0, dy0, dx1, dy1, mask, filter);
    },

    bufferDataWithData(target, bufferdata, bufferlen, usage) {
        const data = new DataView(window.ONYX_MEMORY.buffer, bufferdata, bufferlen);
        gl.bufferData(target, data, usage);
    },

    bufferDataNoData(target, size, usage) { gl.bufferData(target, size, usage); },
    bufferSubData(target, offset, bufferdata, bufferlen) {
        const data = new DataView(window.ONYX_MEMORY.buffer, bufferdata, bufferlen);
        gl.bufferSubData(target, offset, data);
    },
    canvasSize(width, height) {
        canvas.width = width;
        canvas.height = height;
    },
    checkFramebufferStatus(target) { return gl.checkFramebufferStatus(target); },
    clear(bit) { gl.clear(bit); },
    clearColor(r, g, b, a) { gl.clearColor(r, g, b, a); },
    clearDepth(depth) { gl.clearDepth(depth); },
    clearStencil(stencil) { gl.clearStencil(stencil); },
    colorMask(r, g, b, a) { gl.colorMask(r, g, b, a); },
    compileShader(shader) { gl.compileShader(shaders[shader]); },
    compressedTexImage2D(target, level, internalformat, width, height, border, data, datalen) {
        const pixels = new DataView(window.ONYX_MEMORY.buffer, data, datalen);
        gl.compressedTexImage2D(target, level, internalformat, width, height, border, pixels);
    },
    compressedTexSubImage2D(target, level, internalformat, xoff, yoff, width, height, format, data, datalen) {
        const pixels = new DataView(window.ONYX_MEMORY.buffer, data, datalen);
        gl.compressedSubTexImage2D(target, level, internalformat, xoff, yoff, width, height, format, pixels);
    },
    copyBufferSubData(readTarget, writeTarget, readOffset, writeOffset, size) { gl.copyBufferSubData(readTarget, writeTarget, readOffset, writeOffset, size); },
    copyTexImage2D(target, level, internalformat, x, y, width, height, border) {
        gl.copyTexImage2D(target, level, internalformat, x, y, width, height, border);
    },
    copyTexSubImage2D(target, level, xoff, yoff, x, y, width, height) {
        gl.copyTexSubImage2D(target, level, xoff, yoff, x, y, width, height);
    },
    createBuffer() {
        const buf = gl.createBuffer();
        if (buf == null) return -1;

        buffers.push(buf);
        return buffers.length - 1;
    },
    createFramebuffer() {
        const buf = gl.createFramebuffer();
        if (buf == null) return -1;

        framebuffers.push(buf);
        return framebuffers.length - 1;
    },
    createProgram() {
        const prog = gl.createProgram();
        if (prog == null) return -1;

        programs.push(prog);
        return programs.length - 1;
    },
    createRenderbuffer() {
        const buf = gl.createRenderbuffer();
        if (buf == null) return -1;

        renderbuffers.push(buf);
        return renderbuffers.length - 1;
    },
    createShader(type) {
        const shader = gl.createShader(type);
        if (shader == null) return -1;

        shaders.push(shader);
        return shaders.length - 1;
    },
    createTexture() {
        const texture = gl.createTexture();
        if (texture == null) return -1;

        textures.push(texture);
        return textures.length - 1;
    },
    createVertexArray() {
        const vao = gl.createVertexArray();
        if (vao == null) return -1;

        vertexArrays.push(vao);
        return vertexArrays.length - 1;
    },
    cullFace(mode) { gl.cullFace(mode); },
    deleteBuffer(buffer) { gl.deleteBuffer(buffers[buffer]); },
    deleteFramebuffer(framebuffer) { gl.deleteFramebuffer(framebuffers[framebuffer]); },
    deleteProgram(program) { gl.deleteProgram(programs[program]); },
    deleteRenderbuffer(renderbuffer) { gl.deleteRenderbuffer(renderbuffers[renderbuffer]); },
    deleteShader(shader) { gl.deleteShader(shaders[shader]); },
    deleteTexture(texture) { gl.deleteTexture(textures[texture]); },
    deleteVertexArray(vertexArray) { gl.deleteVertexArray(vertexArrays[vertexArray]); },
    depthFunc(func) { gl.depthFunc(func); },
    depthMask(flag) { gl.depthMask(flag); },
    depthRange(znear, zfar) { gl.depthRange(znear, zfar); },
    detachShader(program, shader) { gl.detachShader(programs[program], shaders[shader]); },
    disable(cap) { gl.disable(cap); },
    disableVertexAttribArray(index) { gl.disableVertexAttribArray(index); },
    drawArrays(mode, first, count) { gl.drawArrays(mode, first, count); },
    drawArraysInstanced(mode, first, count, instanceCount) { gl.drawArraysInstanced(mode, first, count, instanceCount); },
    drawElements(mode, count, type, offset) { gl.drawElements(mode, count, type, offset); },
    drawElementsInstanced(mode, count, type, offset, instanceCount) { gl.drawElementsInstanced(mode, count, type, offset, instanceCount); },
    enable(cap) { gl.enable(cap); },
    enableVertexAttribArray(index) { gl.enableVertexAttribArray(index); },
    finish() { gl.finish(); },
    flush() { gl.flush(); },
    framebufferRenderbuffer(target, attachment, renderbuffertarget, renderbuffer) {
        gl.framebufferRenderbuffer(target, attachment, renderbuffertarget, renderbuffers[renderbuffer]);
    },
    framebufferTexture2D(target, attachment, texttarget, texture, level) {
        gl.framebufferTexture2D(target, attachment, texttarget, textures[texture], level);
    },
    framebufferTextureLayer(target, attachment, texture, level, layer) { gl.framebufferTextureLayer(target, attachment, textures[texture], level, layer); },
    frontFace(mode) { gl.frontFace(mode); },
    generateMipmap(target) { gl.generateMipmap(target); },
    getActiveAttrib(program, index, out) {
        const loc = gl.getActiveAttrib(programs[program], index);
        const data = new Int32Array(window.ONYX_MEMORY.buffer, out, 2);
        data[0] = loc.size;
        data[1] = loc.type;
    },
    getActiveUniform(program, index, out) {
        const loc = gl.getActiveUniform(programs[program], index);
        const data = new Int32Array(window.ONYX_MEMORY.buffer, out, 2);
        data[0] = loc.size;
        data[1] = loc.type;
    },
    // getAttachedShaders() { console.log("NOT IMPLEMENTED!"); },
    getAttribLocation(program, name, namelen) {
        const attribname = onyx_decode_text(name, namelen);

        return gl.getAttribLocation(programs[program], attribname);
    },
    // getBufferParameter() { console.log("NOT IMPLEMENTED!"); },
    getBufferSubData(target, srcbyteoffset, dstbufferdata, dstbufferlen, dstoffset, length) {
        const dst = new DataView(window.ONYX_MEMORY.buffer, dstbufferdata, dstbufferlen);
        gl.getBufferSubData(target, srcbyteoffset, dst, dstoffset, length);
    },
    getError() { return gl.getError(); },
    getInternalformatParameter(target, internalformat, pname) { return gl.getInternalformatParameter(target, internalformat, pname); },
    // many of the 'gets() { console.log("NOT IMPLEMENTED!"); },
    getShaderParameter(shader, param) { return gl.getShaderParameter(shaders[shader], param); },
    getProgramParameter(program, param) { return gl.getProgramParameter(programs[program], param); },
    getUniformLocation(program, name, namelen) {
        const uniname = onyx_decode_text(name, namelen);

        uniformlocs.push(gl.getUniformLocation(programs[program], uniname));
        return uniformlocs.length - 1;
    },
    getVertexAttribOffset(index, pname) { return gl.getVertexAttribOffset(index, pname); },
    hint(target, mode) { gl.hint(target, mode); },
    isEnabled(cap) { return gl.isEnabled(cap); },
    invalidateFramebuffer(target, attachdata, attachlen) {
        const attachments = new Int32Array(window.ONYX_MEMORY.buffer, attachdata, attachlen);
        gl.invalidateFramebuffer(target, attacements);
    },
    invalidateSubFramebuffer(target, attachdata, attachlen, x, y, width, height) {
        const attachments = new Int32Array(window.ONYX_MEMORY.buffer, attachdata, attachlen);
        gl.invalidateFramebuffer(target, attacements, x, y, width, height);
    },
    lineWidth(width) { gl.lineWidth(width); },
    linkProgram(program) { gl.linkProgram(programs[program]); },
    pixelStorei(pname, param) { gl.pixelStorei(pname, param); },
    polygonOffset(factor, units) { gl.polygonOffset(factor, units); },
    printProgramInfoLog(program) { console.log(gl.getProgramInfoLog(programs[program])); },
    printShaderInfoLog(shader) { console.log(gl.getShaderInfoLog(shaders[shader])); },
    readPixels(x, y, width, height, format, type, pixels, pixelslen) {
        const pixeldata = new Uint8Array(window.ONYX_MEMORY.buffer, pixels, pixelslen);
        gl.readPixels(x, y, width, height, format, type, pixeldata);
    },
    readBuffer(src) { gl.readBuffer(src); },
    renderbufferStorage(target, internalformat, width, height) { gl.renderbufferStorage(target, internalformat, width, height); },
    renderbufferStorageMultisample(target, samples, internalformat, width, height) {
        gl.renderbufferStorageMultisample(target, samples, internalformat, width, height);
    },
    sampleCoverage(value, invert) { gl.sampleCoverage(value, invert); },
    scissor(x, y, width, height) { gl.scissor(x, y, width, height); },
    setSize(width, height) { canvas.width = width; canvas.height = height; },
    shaderSource(shader, source, sourcelen) {
        const sourcedata = onyx_decode_text(source, sourcelen);

        gl.shaderSource(shaders[shader], sourcedata);
    },
    stencilFunc(func, ref, mask) { gl.stencilFunc(func, ref, mask); },
    stencilFuncSeparate(face, func, ref, mask) { gl.stencilFuncSeparate(face, func, ref, mask); },
    stencilMask(mask) { gl.stencilMask(mask); },
    stencilMaskSeparate(face, mask) { gl.stencilMaskSeparate(face, mask); },
    stencilOp(fail, zfail, mask) { gl.stencilOp(fail, zfail, mask); },
    stencilOpSeparate(face, fail, zfail, zpass) { gl.stencilOpSeparate(face, fail, zfail, zpass); },
    texImage2D(target, level, internalformat, width, height, border, format, type, pixels, pixelslen) {
        const data = new Uint8Array(window.ONYX_MEMORY.buffer, pixels, pixelslen);
        gl.texImage2D(target, level, internalformat, width, height, border, format, type, data);
    },
    texParameterf(target, pname, param) { gl.texParameterf(target, pname, param); },
    texParameteri(target, pname, param) { gl.texParameteri(target, pname, param); },
    texStorage2D(target, levels, internalformat, width, height) { gl.texStorage2D(target, levels, internalformat, width, height); },
    texSubImage2D(target, level, xoff, yoff, width, height, format, type, pixels, pixelslen) {
        const data = new Uint8Array(window.ONYX_MEMORY.buffer, pixels, pixelslen);
        gl.texSubImage2D(target, level, xoff, yoff, width, height, format, type, data);
    },
    uniform1f(loc, x) { gl.uniform1f(uniformlocs[loc], x); },
    uniform1i(loc, x) { gl.uniform1i(uniformlocs[loc], x); },
    uniform2f(loc, x, y) { gl.uniform2f(uniformlocs[loc], x, y); },
    uniform2i(loc, x, y) { gl.uniform2i(uniformlocs[loc], x, y); },
    uniform3f(loc, x, y, z) { gl.uniform3f(uniformlocs[loc], x, y, z); },
    uniform3i(loc, x, y, z) { gl.uniform3i(uniformlocs[loc], x, y, z); },
    uniform4f(loc, x, y, z, w) { gl.uniform4f(uniformlocs[loc], x, y, z, w); },
    uniform4i(loc, x, y, z, w) { gl.uniform4i(uniformlocs[loc], x, y, z, w); },
    uniformMatrix2(loc, transpose, valueptr) {
        const data = new Float32Array(window.ONYX_MEMORY.buffer, valueptr, 4);
        gl.uniformMatrix2fv(uniformlocs[loc], transpose, data);
    },
    uniformMatrix3(loc, transpose, valueptr) {
        const data = new Float32Array(window.ONYX_MEMORY.buffer, valueptr, 9);
        gl.uniformMatrix3fv(uniformlocs[loc], transpose, data);
    },
    uniformMatrix4(loc, transpose, valueptr) {
        const data = new Float32Array(window.ONYX_MEMORY.buffer, valueptr, 16);
        gl.uniformMatrix4fv(uniformlocs[loc], transpose, data);
    },
    useProgram(program) { gl.useProgram(programs[program]); },
    validateProgram(program) { gl.validateProgram(program[program]); },
    vertexAttrib1f(idx, x) { gl.vertexAttrib1f(idx, x); },
    vertexAttrib2f(idx, x, y) { gl.vertexAttrib2f(idx, x, y); },
    vertexAttrib3f(idx, x, y, z) { gl.vertexAttrib3f(idx, x, y, z); },
    vertexAttrib4f(idx, x, y, z, w) { gl.vertexAttrib4f(idx, x, y, z, w); },
    vertexAttribIPointer(idx, size, type, stride, offset) { gl.vertexAttribIPointer(idx, size, type, stride, offset); },
    vertexAttribPointer(idx, size, type, normalized, stride, offset) { gl.vertexAttribPointer(idx, size, type, normalized, stride, offset); },
    vertexAttribDivisor(idx, divisor) { gl.vertexAttribDivisor(idx, divisor); },
    viewport(x, y, width, height) { gl.viewport(x, y, width, height); },
});
