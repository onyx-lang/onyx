//
// THIS FILE WAS AUTOMATICALLY GENERATED.
//

#define GLAD_GLES2_IMPLEMENTATION
#include "glad.h"


#define ONYX_LIBRARY_NAME onyx_opengles
#include "onyx_library.h"

#define P(i, k) (params->data[i].of.k)


ONYX_DEF(glInit, (LONG), ()) {
    GLADloadfunc load_sym = (GLADloadfunc) params->data[0].of.i64;
    gladLoadGLES2(load_sym);
    return NULL;
}
            
ONYX_DEF(glActiveTexture, (WASM_I32), ()) {
    glad_glActiveTexture(P(0, i32));
    return NULL;
}

ONYX_DEF(glAttachShader, (WASM_I32, WASM_I32), ()) {
    glad_glAttachShader(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glBindAttribLocation, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glBindAttribLocation(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glBindBuffer, (WASM_I32, WASM_I32), ()) {
    glad_glBindBuffer(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glBindFramebuffer, (WASM_I32, WASM_I32), ()) {
    glad_glBindFramebuffer(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glBindRenderbuffer, (WASM_I32, WASM_I32), ()) {
    glad_glBindRenderbuffer(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glBindTexture, (WASM_I32, WASM_I32), ()) {
    glad_glBindTexture(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glBlendColor, (WASM_F32, WASM_F32, WASM_F32, WASM_F32), ()) {
    glad_glBlendColor(P(0, f32), P(1, f32), P(2, f32), P(3, f32));
    return NULL;
}

ONYX_DEF(glBlendEquation, (WASM_I32), ()) {
    glad_glBlendEquation(P(0, i32));
    return NULL;
}

ONYX_DEF(glBlendEquationSeparate, (WASM_I32, WASM_I32), ()) {
    glad_glBlendEquationSeparate(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glBlendFunc, (WASM_I32, WASM_I32), ()) {
    glad_glBlendFunc(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glBlendFuncSeparate, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glBlendFuncSeparate(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glBufferData, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glBufferData(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32));
    return NULL;
}

ONYX_DEF(glBufferSubData, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glBufferSubData(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glCheckFramebufferStatus, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glCheckFramebufferStatus(P(0, i32)));
    return NULL;
}

ONYX_DEF(glClear, (WASM_I32), ()) {
    glad_glClear(P(0, i32));
    return NULL;
}

ONYX_DEF(glClearColor, (WASM_F32, WASM_F32, WASM_F32, WASM_F32), ()) {
    glad_glClearColor(P(0, f32), P(1, f32), P(2, f32), P(3, f32));
    return NULL;
}

ONYX_DEF(glClearDepthf, (WASM_F32), ()) {
    glad_glClearDepthf(P(0, f32));
    return NULL;
}

ONYX_DEF(glClearStencil, (WASM_I32), ()) {
    glad_glClearStencil(P(0, i32));
    return NULL;
}

ONYX_DEF(glColorMask, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glColorMask(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glCompileShader, (WASM_I32), ()) {
    glad_glCompileShader(P(0, i32));
    return NULL;
}

ONYX_DEF(glCompressedTexImage2D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glCompressedTexImage2D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), ONYX_PTR(P(7, i32)));
    return NULL;
}

ONYX_DEF(glCompressedTexSubImage2D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glCompressedTexSubImage2D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32), ONYX_PTR(P(8, i32)));
    return NULL;
}

ONYX_DEF(glCopyTexImage2D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glCopyTexImage2D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32));
    return NULL;
}

ONYX_DEF(glCopyTexSubImage2D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glCopyTexSubImage2D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32));
    return NULL;
}

ONYX_DEF(glCreateProgram, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glCreateProgram());
    return NULL;
}

ONYX_DEF(glCreateShader, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glCreateShader(P(0, i32)));
    return NULL;
}

ONYX_DEF(glCullFace, (WASM_I32), ()) {
    glad_glCullFace(P(0, i32));
    return NULL;
}

ONYX_DEF(glDeleteBuffers, (WASM_I32, WASM_I32), ()) {
    glad_glDeleteBuffers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glDeleteFramebuffers, (WASM_I32, WASM_I32), ()) {
    glad_glDeleteFramebuffers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glDeleteProgram, (WASM_I32), ()) {
    glad_glDeleteProgram(P(0, i32));
    return NULL;
}

ONYX_DEF(glDeleteRenderbuffers, (WASM_I32, WASM_I32), ()) {
    glad_glDeleteRenderbuffers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glDeleteShader, (WASM_I32), ()) {
    glad_glDeleteShader(P(0, i32));
    return NULL;
}

ONYX_DEF(glDeleteTextures, (WASM_I32, WASM_I32), ()) {
    glad_glDeleteTextures(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glDepthFunc, (WASM_I32), ()) {
    glad_glDepthFunc(P(0, i32));
    return NULL;
}

ONYX_DEF(glDepthMask, (WASM_I32), ()) {
    glad_glDepthMask(P(0, i32));
    return NULL;
}

ONYX_DEF(glDepthRangef, (WASM_F32, WASM_F32), ()) {
    glad_glDepthRangef(P(0, f32), P(1, f32));
    return NULL;
}

ONYX_DEF(glDetachShader, (WASM_I32, WASM_I32), ()) {
    glad_glDetachShader(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glDisable, (WASM_I32), ()) {
    glad_glDisable(P(0, i32));
    return NULL;
}

ONYX_DEF(glDisableVertexAttribArray, (WASM_I32), ()) {
    glad_glDisableVertexAttribArray(P(0, i32));
    return NULL;
}

ONYX_DEF(glDrawArrays, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glDrawArrays(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glDrawElements, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glDrawElements(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glEnable, (WASM_I32), ()) {
    glad_glEnable(P(0, i32));
    return NULL;
}

ONYX_DEF(glEnableVertexAttribArray, (WASM_I32), ()) {
    glad_glEnableVertexAttribArray(P(0, i32));
    return NULL;
}

ONYX_DEF(glFinish, (), ()) {
    glad_glFinish();
    return NULL;
}

ONYX_DEF(glFlush, (), ()) {
    glad_glFlush();
    return NULL;
}

ONYX_DEF(glFramebufferRenderbuffer, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glFramebufferRenderbuffer(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glFramebufferTexture2D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glFramebufferTexture2D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glFrontFace, (WASM_I32), ()) {
    glad_glFrontFace(P(0, i32));
    return NULL;
}

ONYX_DEF(glGenBuffers, (WASM_I32, WASM_I32), ()) {
    glad_glGenBuffers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGenerateMipmap, (WASM_I32), ()) {
    glad_glGenerateMipmap(P(0, i32));
    return NULL;
}

ONYX_DEF(glGenFramebuffers, (WASM_I32, WASM_I32), ()) {
    glad_glGenFramebuffers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGenRenderbuffers, (WASM_I32, WASM_I32), ()) {
    glad_glGenRenderbuffers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGenTextures, (WASM_I32, WASM_I32), ()) {
    glad_glGenTextures(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGetActiveAttrib, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetActiveAttrib(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)), ONYX_PTR(P(5, i32)), ONYX_PTR(P(6, i32)));
    return NULL;
}

ONYX_DEF(glGetActiveUniform, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetActiveUniform(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)), ONYX_PTR(P(5, i32)), ONYX_PTR(P(6, i32)));
    return NULL;
}

ONYX_DEF(glGetAttachedShaders, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetAttachedShaders(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glGetAttribLocation, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glGetAttribLocation(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(glGetBooleanv, (WASM_I32, WASM_I32), ()) {
    glad_glGetBooleanv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGetBufferParameteriv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetBufferParameteriv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetError, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glGetError());
    return NULL;
}

ONYX_DEF(glGetFloatv, (WASM_I32, WASM_I32), ()) {
    glad_glGetFloatv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGetFramebufferAttachmentParameteriv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetFramebufferAttachmentParameteriv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glGetIntegerv, (WASM_I32, WASM_I32), ()) {
    glad_glGetIntegerv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGetProgramiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetProgramiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetProgramInfoLog, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetProgramInfoLog(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glGetRenderbufferParameteriv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetRenderbufferParameteriv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetShaderiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetShaderiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetShaderInfoLog, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetShaderInfoLog(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glGetShaderPrecisionFormat, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetShaderPrecisionFormat(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glGetShaderSource, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetShaderSource(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glGetString, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glad_glGetString(P(0, i32)));
    return NULL;
}

ONYX_DEF(glGetTexParameterfv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetTexParameterfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetTexParameteriv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetTexParameteriv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetUniformfv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetUniformfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetUniformiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetUniformiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetUniformLocation, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glGetUniformLocation(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(glGetVertexAttribfv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetVertexAttribfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetVertexAttribiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetVertexAttribiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetVertexAttribPointerv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetVertexAttribPointerv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glHint, (WASM_I32, WASM_I32), ()) {
    glad_glHint(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glIsBuffer, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsBuffer(P(0, i32)));
    return NULL;
}

ONYX_DEF(glIsEnabled, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsEnabled(P(0, i32)));
    return NULL;
}

ONYX_DEF(glIsFramebuffer, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsFramebuffer(P(0, i32)));
    return NULL;
}

ONYX_DEF(glIsProgram, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsProgram(P(0, i32)));
    return NULL;
}

ONYX_DEF(glIsRenderbuffer, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsRenderbuffer(P(0, i32)));
    return NULL;
}

ONYX_DEF(glIsShader, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsShader(P(0, i32)));
    return NULL;
}

ONYX_DEF(glIsTexture, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsTexture(P(0, i32)));
    return NULL;
}

ONYX_DEF(glLineWidth, (WASM_F32), ()) {
    glad_glLineWidth(P(0, f32));
    return NULL;
}

ONYX_DEF(glLinkProgram, (WASM_I32), ()) {
    glad_glLinkProgram(P(0, i32));
    return NULL;
}

ONYX_DEF(glPixelStorei, (WASM_I32, WASM_I32), ()) {
    glad_glPixelStorei(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glPolygonOffset, (WASM_F32, WASM_F32), ()) {
    glad_glPolygonOffset(P(0, f32), P(1, f32));
    return NULL;
}

ONYX_DEF(glReadPixels, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glReadPixels(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), ONYX_PTR(P(6, i32)));
    return NULL;
}

ONYX_DEF(glReleaseShaderCompiler, (), ()) {
    glad_glReleaseShaderCompiler();
    return NULL;
}

ONYX_DEF(glRenderbufferStorage, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glRenderbufferStorage(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glSampleCoverage, (WASM_F32, WASM_I32), ()) {
    glad_glSampleCoverage(P(0, f32), P(1, i32));
    return NULL;
}

ONYX_DEF(glScissor, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glScissor(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glShaderBinary, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glShaderBinary(P(0, i32), ONYX_PTR(P(1, i32)), P(2, i32), ONYX_PTR(P(3, i32)), P(4, i32));
    return NULL;
}

ONYX_DEF(glShaderSource, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glShaderSource(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glStencilFunc, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glStencilFunc(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glStencilFuncSeparate, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glStencilFuncSeparate(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glStencilMask, (WASM_I32), ()) {
    glad_glStencilMask(P(0, i32));
    return NULL;
}

ONYX_DEF(glStencilMaskSeparate, (WASM_I32, WASM_I32), ()) {
    glad_glStencilMaskSeparate(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glStencilOp, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glStencilOp(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glStencilOpSeparate, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glStencilOpSeparate(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glTexImage2D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glTexImage2D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32), ONYX_PTR(P(8, i32)));
    return NULL;
}

ONYX_DEF(glTexParameterf, (WASM_I32, WASM_I32, WASM_F32), ()) {
    glad_glTexParameterf(P(0, i32), P(1, i32), P(2, f32));
    return NULL;
}

ONYX_DEF(glTexParameterfv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glTexParameterfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glTexParameteri, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glTexParameteri(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glTexParameteriv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glTexParameteriv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glTexSubImage2D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glTexSubImage2D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32), ONYX_PTR(P(8, i32)));
    return NULL;
}

ONYX_DEF(glUniform1f, (WASM_I32, WASM_F32), ()) {
    glad_glUniform1f(P(0, i32), P(1, f32));
    return NULL;
}

ONYX_DEF(glUniform1fv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform1fv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniform1i, (WASM_I32, WASM_I32), ()) {
    glad_glUniform1i(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glUniform1iv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform1iv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniform2f, (WASM_I32, WASM_F32, WASM_F32), ()) {
    glad_glUniform2f(P(0, i32), P(1, f32), P(2, f32));
    return NULL;
}

ONYX_DEF(glUniform2fv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform2fv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniform2i, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform2i(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glUniform2iv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform2iv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniform3f, (WASM_I32, WASM_F32, WASM_F32, WASM_F32), ()) {
    glad_glUniform3f(P(0, i32), P(1, f32), P(2, f32), P(3, f32));
    return NULL;
}

ONYX_DEF(glUniform3fv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform3fv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniform3i, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform3i(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glUniform3iv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform3iv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniform4f, (WASM_I32, WASM_F32, WASM_F32, WASM_F32, WASM_F32), ()) {
    glad_glUniform4f(P(0, i32), P(1, f32), P(2, f32), P(3, f32), P(4, f32));
    return NULL;
}

ONYX_DEF(glUniform4fv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform4fv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniform4i, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform4i(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glUniform4iv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform4iv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniformMatrix2fv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniformMatrix2fv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glUniformMatrix3fv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniformMatrix3fv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glUniformMatrix4fv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniformMatrix4fv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glUseProgram, (WASM_I32), ()) {
    glad_glUseProgram(P(0, i32));
    return NULL;
}

ONYX_DEF(glValidateProgram, (WASM_I32), ()) {
    glad_glValidateProgram(P(0, i32));
    return NULL;
}

ONYX_DEF(glVertexAttrib1f, (WASM_I32, WASM_F32), ()) {
    glad_glVertexAttrib1f(P(0, i32), P(1, f32));
    return NULL;
}

ONYX_DEF(glVertexAttrib1fv, (WASM_I32, WASM_I32), ()) {
    glad_glVertexAttrib1fv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glVertexAttrib2f, (WASM_I32, WASM_F32, WASM_F32), ()) {
    glad_glVertexAttrib2f(P(0, i32), P(1, f32), P(2, f32));
    return NULL;
}

ONYX_DEF(glVertexAttrib2fv, (WASM_I32, WASM_I32), ()) {
    glad_glVertexAttrib2fv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glVertexAttrib3f, (WASM_I32, WASM_F32, WASM_F32, WASM_F32), ()) {
    glad_glVertexAttrib3f(P(0, i32), P(1, f32), P(2, f32), P(3, f32));
    return NULL;
}

ONYX_DEF(glVertexAttrib3fv, (WASM_I32, WASM_I32), ()) {
    glad_glVertexAttrib3fv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glVertexAttrib4f, (WASM_I32, WASM_F32, WASM_F32, WASM_F32, WASM_F32), ()) {
    glad_glVertexAttrib4f(P(0, i32), P(1, f32), P(2, f32), P(3, f32), P(4, f32));
    return NULL;
}

ONYX_DEF(glVertexAttrib4fv, (WASM_I32, WASM_I32), ()) {
    glad_glVertexAttrib4fv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glVertexAttribPointer, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glVertexAttribPointer(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), ONYX_PTR(P(5, i32)));
    return NULL;
}

ONYX_DEF(glViewport, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glViewport(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glReadBuffer, (WASM_I32), ()) {
    glad_glReadBuffer(P(0, i32));
    return NULL;
}

ONYX_DEF(glDrawRangeElements, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glDrawRangeElements(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), ONYX_PTR(P(5, i32)));
    return NULL;
}

ONYX_DEF(glTexImage3D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glTexImage3D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32), P(8, i32), ONYX_PTR(P(9, i32)));
    return NULL;
}

ONYX_DEF(glTexSubImage3D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glTexSubImage3D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32), P(8, i32), P(9, i32), ONYX_PTR(P(10, i32)));
    return NULL;
}

ONYX_DEF(glCopyTexSubImage3D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glCopyTexSubImage3D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32), P(8, i32));
    return NULL;
}

ONYX_DEF(glCompressedTexImage3D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glCompressedTexImage3D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32), ONYX_PTR(P(8, i32)));
    return NULL;
}

ONYX_DEF(glCompressedTexSubImage3D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glCompressedTexSubImage3D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32), P(8, i32), P(9, i32), ONYX_PTR(P(10, i32)));
    return NULL;
}

ONYX_DEF(glGenQueries, (WASM_I32, WASM_I32), ()) {
    glad_glGenQueries(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glDeleteQueries, (WASM_I32, WASM_I32), ()) {
    glad_glDeleteQueries(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glIsQuery, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsQuery(P(0, i32)));
    return NULL;
}

ONYX_DEF(glBeginQuery, (WASM_I32, WASM_I32), ()) {
    glad_glBeginQuery(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glEndQuery, (WASM_I32), ()) {
    glad_glEndQuery(P(0, i32));
    return NULL;
}

ONYX_DEF(glGetQueryiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetQueryiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetQueryObjectuiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetQueryObjectuiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUnmapBuffer, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glUnmapBuffer(P(0, i32)));
    return NULL;
}

ONYX_DEF(glGetBufferPointerv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetBufferPointerv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glDrawBuffers, (WASM_I32, WASM_I32), ()) {
    glad_glDrawBuffers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glUniformMatrix2x3fv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniformMatrix2x3fv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glUniformMatrix3x2fv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniformMatrix3x2fv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glUniformMatrix2x4fv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniformMatrix2x4fv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glUniformMatrix4x2fv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniformMatrix4x2fv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glUniformMatrix3x4fv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniformMatrix3x4fv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glUniformMatrix4x3fv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniformMatrix4x3fv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glBlitFramebuffer, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glBlitFramebuffer(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32), P(8, i32), P(9, i32));
    return NULL;
}

ONYX_DEF(glRenderbufferStorageMultisample, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glRenderbufferStorageMultisample(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glFramebufferTextureLayer, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glFramebufferTextureLayer(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glMapBufferRange, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glMapBufferRange(P(0, i32), P(1, i32), P(2, i32), P(3, i32)));
    return NULL;
}

ONYX_DEF(glFlushMappedBufferRange, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glFlushMappedBufferRange(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glBindVertexArray, (WASM_I32), ()) {
    glad_glBindVertexArray(P(0, i32));
    return NULL;
}

ONYX_DEF(glDeleteVertexArrays, (WASM_I32, WASM_I32), ()) {
    glad_glDeleteVertexArrays(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGenVertexArrays, (WASM_I32, WASM_I32), ()) {
    glad_glGenVertexArrays(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glIsVertexArray, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsVertexArray(P(0, i32)));
    return NULL;
}

ONYX_DEF(glGetIntegeri_v, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetIntegeri_v(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glBeginTransformFeedback, (WASM_I32), ()) {
    glad_glBeginTransformFeedback(P(0, i32));
    return NULL;
}

ONYX_DEF(glEndTransformFeedback, (), ()) {
    glad_glEndTransformFeedback();
    return NULL;
}

ONYX_DEF(glBindBufferRange, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glBindBufferRange(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glBindBufferBase, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glBindBufferBase(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glTransformFeedbackVaryings, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glTransformFeedbackVaryings(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32));
    return NULL;
}

ONYX_DEF(glGetTransformFeedbackVarying, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetTransformFeedbackVarying(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)), ONYX_PTR(P(5, i32)), ONYX_PTR(P(6, i32)));
    return NULL;
}

ONYX_DEF(glVertexAttribIPointer, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glVertexAttribIPointer(P(0, i32), P(1, i32), P(2, i32), P(3, i32), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(glGetVertexAttribIiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetVertexAttribIiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetVertexAttribIuiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetVertexAttribIuiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glVertexAttribI4i, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glVertexAttribI4i(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glVertexAttribI4ui, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glVertexAttribI4ui(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glVertexAttribI4iv, (WASM_I32, WASM_I32), ()) {
    glad_glVertexAttribI4iv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glVertexAttribI4uiv, (WASM_I32, WASM_I32), ()) {
    glad_glVertexAttribI4uiv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGetUniformuiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetUniformuiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetFragDataLocation, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glGetFragDataLocation(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(glUniform1ui, (WASM_I32, WASM_I32), ()) {
    glad_glUniform1ui(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glUniform2ui, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform2ui(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glUniform3ui, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform3ui(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glUniform4ui, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform4ui(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glUniform1uiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform1uiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniform2uiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform2uiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniform3uiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform3uiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glUniform4uiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniform4uiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glClearBufferiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glClearBufferiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glClearBufferuiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glClearBufferuiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glClearBufferfv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glClearBufferfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glClearBufferfi, (WASM_I32, WASM_I32, WASM_F32, WASM_I32), ()) {
    glad_glClearBufferfi(P(0, i32), P(1, i32), P(2, f32), P(3, i32));
    return NULL;
}

ONYX_DEF(glGetStringi, (WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glad_glGetStringi(P(0, i32), P(1, i32)));
    return NULL;
}

ONYX_DEF(glCopyBufferSubData, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glCopyBufferSubData(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glGetUniformIndices, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetUniformIndices(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glGetActiveUniformsiv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetActiveUniformsiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(glGetUniformBlockIndex, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glGetUniformBlockIndex(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(glGetActiveUniformBlockiv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetActiveUniformBlockiv(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(glGetActiveUniformBlockName, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetActiveUniformBlockName(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(glUniformBlockBinding, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glUniformBlockBinding(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glDrawArraysInstanced, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glDrawArraysInstanced(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(glDrawElementsInstanced, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glDrawElementsInstanced(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)), P(4, i32));
    return NULL;
}

ONYX_DEF(glGetInteger64v, (WASM_I32, WASM_I32), ()) {
    glad_glGetInteger64v(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGetInteger64i_v, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetInteger64i_v(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetBufferParameteri64v, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetBufferParameteri64v(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGenSamplers, (WASM_I32, WASM_I32), ()) {
    glad_glGenSamplers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glDeleteSamplers, (WASM_I32, WASM_I32), ()) {
    glad_glDeleteSamplers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glIsSampler, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsSampler(P(0, i32)));
    return NULL;
}

ONYX_DEF(glBindSampler, (WASM_I32, WASM_I32), ()) {
    glad_glBindSampler(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glSamplerParameteri, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glSamplerParameteri(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glSamplerParameteriv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glSamplerParameteriv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glSamplerParameterf, (WASM_I32, WASM_I32, WASM_F32), ()) {
    glad_glSamplerParameterf(P(0, i32), P(1, i32), P(2, f32));
    return NULL;
}

ONYX_DEF(glSamplerParameterfv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glSamplerParameterfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetSamplerParameteriv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetSamplerParameteriv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glGetSamplerParameterfv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetSamplerParameterfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glVertexAttribDivisor, (WASM_I32, WASM_I32), ()) {
    glad_glVertexAttribDivisor(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glBindTransformFeedback, (WASM_I32, WASM_I32), ()) {
    glad_glBindTransformFeedback(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glDeleteTransformFeedbacks, (WASM_I32, WASM_I32), ()) {
    glad_glDeleteTransformFeedbacks(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glGenTransformFeedbacks, (WASM_I32, WASM_I32), ()) {
    glad_glGenTransformFeedbacks(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glIsTransformFeedback, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glad_glIsTransformFeedback(P(0, i32)));
    return NULL;
}

ONYX_DEF(glPauseTransformFeedback, (), ()) {
    glad_glPauseTransformFeedback();
    return NULL;
}

ONYX_DEF(glResumeTransformFeedback, (), ()) {
    glad_glResumeTransformFeedback();
    return NULL;
}

ONYX_DEF(glGetProgramBinary, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetProgramBinary(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(glProgramBinary, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glProgramBinary(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32));
    return NULL;
}

ONYX_DEF(glProgramParameteri, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glProgramParameteri(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glInvalidateFramebuffer, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glInvalidateFramebuffer(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glInvalidateSubFramebuffer, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glInvalidateSubFramebuffer(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32), P(4, i32), P(5, i32), P(6, i32));
    return NULL;
}

ONYX_DEF(glTexStorage2D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glTexStorage2D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glTexStorage3D, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glTexStorage3D(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32));
    return NULL;
}

ONYX_DEF(glGetInternalformativ, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glad_glGetInternalformativ(P(0, i32), P(1, i32), P(2, i32), P(3, i32), ONYX_PTR(P(4, i32)));
    return NULL;
}



ONYX_LIBRARY {
    ONYX_FUNC(glInit)
    ONYX_FUNC(glActiveTexture)
    ONYX_FUNC(glAttachShader)
    ONYX_FUNC(glBindAttribLocation)
    ONYX_FUNC(glBindBuffer)
    ONYX_FUNC(glBindFramebuffer)
    ONYX_FUNC(glBindRenderbuffer)
    ONYX_FUNC(glBindTexture)
    ONYX_FUNC(glBlendColor)
    ONYX_FUNC(glBlendEquation)
    ONYX_FUNC(glBlendEquationSeparate)
    ONYX_FUNC(glBlendFunc)
    ONYX_FUNC(glBlendFuncSeparate)
    ONYX_FUNC(glBufferData)
    ONYX_FUNC(glBufferSubData)
    ONYX_FUNC(glCheckFramebufferStatus)
    ONYX_FUNC(glClear)
    ONYX_FUNC(glClearColor)
    ONYX_FUNC(glClearDepthf)
    ONYX_FUNC(glClearStencil)
    ONYX_FUNC(glColorMask)
    ONYX_FUNC(glCompileShader)
    ONYX_FUNC(glCompressedTexImage2D)
    ONYX_FUNC(glCompressedTexSubImage2D)
    ONYX_FUNC(glCopyTexImage2D)
    ONYX_FUNC(glCopyTexSubImage2D)
    ONYX_FUNC(glCreateProgram)
    ONYX_FUNC(glCreateShader)
    ONYX_FUNC(glCullFace)
    ONYX_FUNC(glDeleteBuffers)
    ONYX_FUNC(glDeleteFramebuffers)
    ONYX_FUNC(glDeleteProgram)
    ONYX_FUNC(glDeleteRenderbuffers)
    ONYX_FUNC(glDeleteShader)
    ONYX_FUNC(glDeleteTextures)
    ONYX_FUNC(glDepthFunc)
    ONYX_FUNC(glDepthMask)
    ONYX_FUNC(glDepthRangef)
    ONYX_FUNC(glDetachShader)
    ONYX_FUNC(glDisable)
    ONYX_FUNC(glDisableVertexAttribArray)
    ONYX_FUNC(glDrawArrays)
    ONYX_FUNC(glDrawElements)
    ONYX_FUNC(glEnable)
    ONYX_FUNC(glEnableVertexAttribArray)
    ONYX_FUNC(glFinish)
    ONYX_FUNC(glFlush)
    ONYX_FUNC(glFramebufferRenderbuffer)
    ONYX_FUNC(glFramebufferTexture2D)
    ONYX_FUNC(glFrontFace)
    ONYX_FUNC(glGenBuffers)
    ONYX_FUNC(glGenerateMipmap)
    ONYX_FUNC(glGenFramebuffers)
    ONYX_FUNC(glGenRenderbuffers)
    ONYX_FUNC(glGenTextures)
    ONYX_FUNC(glGetActiveAttrib)
    ONYX_FUNC(glGetActiveUniform)
    ONYX_FUNC(glGetAttachedShaders)
    ONYX_FUNC(glGetAttribLocation)
    ONYX_FUNC(glGetBooleanv)
    ONYX_FUNC(glGetBufferParameteriv)
    ONYX_FUNC(glGetError)
    ONYX_FUNC(glGetFloatv)
    ONYX_FUNC(glGetFramebufferAttachmentParameteriv)
    ONYX_FUNC(glGetIntegerv)
    ONYX_FUNC(glGetProgramiv)
    ONYX_FUNC(glGetProgramInfoLog)
    ONYX_FUNC(glGetRenderbufferParameteriv)
    ONYX_FUNC(glGetShaderiv)
    ONYX_FUNC(glGetShaderInfoLog)
    ONYX_FUNC(glGetShaderPrecisionFormat)
    ONYX_FUNC(glGetShaderSource)
    ONYX_FUNC(glGetString)
    ONYX_FUNC(glGetTexParameterfv)
    ONYX_FUNC(glGetTexParameteriv)
    ONYX_FUNC(glGetUniformfv)
    ONYX_FUNC(glGetUniformiv)
    ONYX_FUNC(glGetUniformLocation)
    ONYX_FUNC(glGetVertexAttribfv)
    ONYX_FUNC(glGetVertexAttribiv)
    ONYX_FUNC(glGetVertexAttribPointerv)
    ONYX_FUNC(glHint)
    ONYX_FUNC(glIsBuffer)
    ONYX_FUNC(glIsEnabled)
    ONYX_FUNC(glIsFramebuffer)
    ONYX_FUNC(glIsProgram)
    ONYX_FUNC(glIsRenderbuffer)
    ONYX_FUNC(glIsShader)
    ONYX_FUNC(glIsTexture)
    ONYX_FUNC(glLineWidth)
    ONYX_FUNC(glLinkProgram)
    ONYX_FUNC(glPixelStorei)
    ONYX_FUNC(glPolygonOffset)
    ONYX_FUNC(glReadPixels)
    ONYX_FUNC(glReleaseShaderCompiler)
    ONYX_FUNC(glRenderbufferStorage)
    ONYX_FUNC(glSampleCoverage)
    ONYX_FUNC(glScissor)
    ONYX_FUNC(glShaderBinary)
    ONYX_FUNC(glShaderSource)
    ONYX_FUNC(glStencilFunc)
    ONYX_FUNC(glStencilFuncSeparate)
    ONYX_FUNC(glStencilMask)
    ONYX_FUNC(glStencilMaskSeparate)
    ONYX_FUNC(glStencilOp)
    ONYX_FUNC(glStencilOpSeparate)
    ONYX_FUNC(glTexImage2D)
    ONYX_FUNC(glTexParameterf)
    ONYX_FUNC(glTexParameterfv)
    ONYX_FUNC(glTexParameteri)
    ONYX_FUNC(glTexParameteriv)
    ONYX_FUNC(glTexSubImage2D)
    ONYX_FUNC(glUniform1f)
    ONYX_FUNC(glUniform1fv)
    ONYX_FUNC(glUniform1i)
    ONYX_FUNC(glUniform1iv)
    ONYX_FUNC(glUniform2f)
    ONYX_FUNC(glUniform2fv)
    ONYX_FUNC(glUniform2i)
    ONYX_FUNC(glUniform2iv)
    ONYX_FUNC(glUniform3f)
    ONYX_FUNC(glUniform3fv)
    ONYX_FUNC(glUniform3i)
    ONYX_FUNC(glUniform3iv)
    ONYX_FUNC(glUniform4f)
    ONYX_FUNC(glUniform4fv)
    ONYX_FUNC(glUniform4i)
    ONYX_FUNC(glUniform4iv)
    ONYX_FUNC(glUniformMatrix2fv)
    ONYX_FUNC(glUniformMatrix3fv)
    ONYX_FUNC(glUniformMatrix4fv)
    ONYX_FUNC(glUseProgram)
    ONYX_FUNC(glValidateProgram)
    ONYX_FUNC(glVertexAttrib1f)
    ONYX_FUNC(glVertexAttrib1fv)
    ONYX_FUNC(glVertexAttrib2f)
    ONYX_FUNC(glVertexAttrib2fv)
    ONYX_FUNC(glVertexAttrib3f)
    ONYX_FUNC(glVertexAttrib3fv)
    ONYX_FUNC(glVertexAttrib4f)
    ONYX_FUNC(glVertexAttrib4fv)
    ONYX_FUNC(glVertexAttribPointer)
    ONYX_FUNC(glViewport)
    ONYX_FUNC(glReadBuffer)
    ONYX_FUNC(glDrawRangeElements)
    ONYX_FUNC(glTexImage3D)
    ONYX_FUNC(glTexSubImage3D)
    ONYX_FUNC(glCopyTexSubImage3D)
    ONYX_FUNC(glCompressedTexImage3D)
    ONYX_FUNC(glCompressedTexSubImage3D)
    ONYX_FUNC(glGenQueries)
    ONYX_FUNC(glDeleteQueries)
    ONYX_FUNC(glIsQuery)
    ONYX_FUNC(glBeginQuery)
    ONYX_FUNC(glEndQuery)
    ONYX_FUNC(glGetQueryiv)
    ONYX_FUNC(glGetQueryObjectuiv)
    ONYX_FUNC(glUnmapBuffer)
    ONYX_FUNC(glGetBufferPointerv)
    ONYX_FUNC(glDrawBuffers)
    ONYX_FUNC(glUniformMatrix2x3fv)
    ONYX_FUNC(glUniformMatrix3x2fv)
    ONYX_FUNC(glUniformMatrix2x4fv)
    ONYX_FUNC(glUniformMatrix4x2fv)
    ONYX_FUNC(glUniformMatrix3x4fv)
    ONYX_FUNC(glUniformMatrix4x3fv)
    ONYX_FUNC(glBlitFramebuffer)
    ONYX_FUNC(glRenderbufferStorageMultisample)
    ONYX_FUNC(glFramebufferTextureLayer)
    ONYX_FUNC(glMapBufferRange)
    ONYX_FUNC(glFlushMappedBufferRange)
    ONYX_FUNC(glBindVertexArray)
    ONYX_FUNC(glDeleteVertexArrays)
    ONYX_FUNC(glGenVertexArrays)
    ONYX_FUNC(glIsVertexArray)
    ONYX_FUNC(glGetIntegeri_v)
    ONYX_FUNC(glBeginTransformFeedback)
    ONYX_FUNC(glEndTransformFeedback)
    ONYX_FUNC(glBindBufferRange)
    ONYX_FUNC(glBindBufferBase)
    ONYX_FUNC(glTransformFeedbackVaryings)
    ONYX_FUNC(glGetTransformFeedbackVarying)
    ONYX_FUNC(glVertexAttribIPointer)
    ONYX_FUNC(glGetVertexAttribIiv)
    ONYX_FUNC(glGetVertexAttribIuiv)
    ONYX_FUNC(glVertexAttribI4i)
    ONYX_FUNC(glVertexAttribI4ui)
    ONYX_FUNC(glVertexAttribI4iv)
    ONYX_FUNC(glVertexAttribI4uiv)
    ONYX_FUNC(glGetUniformuiv)
    ONYX_FUNC(glGetFragDataLocation)
    ONYX_FUNC(glUniform1ui)
    ONYX_FUNC(glUniform2ui)
    ONYX_FUNC(glUniform3ui)
    ONYX_FUNC(glUniform4ui)
    ONYX_FUNC(glUniform1uiv)
    ONYX_FUNC(glUniform2uiv)
    ONYX_FUNC(glUniform3uiv)
    ONYX_FUNC(glUniform4uiv)
    ONYX_FUNC(glClearBufferiv)
    ONYX_FUNC(glClearBufferuiv)
    ONYX_FUNC(glClearBufferfv)
    ONYX_FUNC(glClearBufferfi)
    ONYX_FUNC(glGetStringi)
    ONYX_FUNC(glCopyBufferSubData)
    ONYX_FUNC(glGetUniformIndices)
    ONYX_FUNC(glGetActiveUniformsiv)
    ONYX_FUNC(glGetUniformBlockIndex)
    ONYX_FUNC(glGetActiveUniformBlockiv)
    ONYX_FUNC(glGetActiveUniformBlockName)
    ONYX_FUNC(glUniformBlockBinding)
    ONYX_FUNC(glDrawArraysInstanced)
    ONYX_FUNC(glDrawElementsInstanced)
    ONYX_FUNC(glGetInteger64v)
    ONYX_FUNC(glGetInteger64i_v)
    ONYX_FUNC(glGetBufferParameteri64v)
    ONYX_FUNC(glGenSamplers)
    ONYX_FUNC(glDeleteSamplers)
    ONYX_FUNC(glIsSampler)
    ONYX_FUNC(glBindSampler)
    ONYX_FUNC(glSamplerParameteri)
    ONYX_FUNC(glSamplerParameteriv)
    ONYX_FUNC(glSamplerParameterf)
    ONYX_FUNC(glSamplerParameterfv)
    ONYX_FUNC(glGetSamplerParameteriv)
    ONYX_FUNC(glGetSamplerParameterfv)
    ONYX_FUNC(glVertexAttribDivisor)
    ONYX_FUNC(glBindTransformFeedback)
    ONYX_FUNC(glDeleteTransformFeedbacks)
    ONYX_FUNC(glGenTransformFeedbacks)
    ONYX_FUNC(glIsTransformFeedback)
    ONYX_FUNC(glPauseTransformFeedback)
    ONYX_FUNC(glResumeTransformFeedback)
    ONYX_FUNC(glGetProgramBinary)
    ONYX_FUNC(glProgramBinary)
    ONYX_FUNC(glProgramParameteri)
    ONYX_FUNC(glInvalidateFramebuffer)
    ONYX_FUNC(glInvalidateSubFramebuffer)
    ONYX_FUNC(glTexStorage2D)
    ONYX_FUNC(glTexStorage3D)
    ONYX_FUNC(glGetInternalformativ)
    NULL
};