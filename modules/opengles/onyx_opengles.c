#include "onyx_library.h"
#include <GLES3/gl3.h>

#define ONYX_LIBRARY_NAME onyx_opengles

#define ONYX_GL_0(name) \
    ONYX_DEF(name, (), ()) { \
        name (); \
        return NULL;                   \
    }

#define ONYX_GL_0_RET_INT(name) \
    ONYX_DEF(name, (), ()) { \
        results->data[0] = WASM_I32_VAL( name () ); \
        return NULL;                   \
    }

#define ONYX_GL_INT_1(name) \
    ONYX_DEF(name, (INT), ()) { \
        name (params->data[0].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_1_RET_INT(name)             \
    ONYX_DEF(name, (INT), (INT)) {              \
        results->data[0] = WASM_I32_VAL( name (params->data[0].of.i32) ); \
    }

#define ONYX_GL_INT_2(name) \
    ONYX_DEF(name, (INT, INT), ()) { \
        name (params->data[0].of.i32, params->data[1].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_2_RET_INT(name) \
    ONYX_DEF(name, (INT, INT), (INT)) { \
        results->data[0] = WASM_I32_VAL( name (params->data[0].of.i32, params->data[1].of.i32) ); \
        return NULL;                   \
    }

#define ONYX_GL_INT_3(name) \
    ONYX_DEF(name, (INT, INT, INT), ()) { \
        name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_4(name) \
    ONYX_DEF(name, (INT, INT, INT, INT), ()) { \
        name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_FLOAT_1(name)                   \
    ONYX_DEF(name, (FLOAT), ()) {  \
        name (params->data[0].of.f32); \
        return NULL; \
    }

#define ONYX_GL_FLOAT_2(name)                   \
    ONYX_DEF(name, (FLOAT, FLOAT), ()) {  \
        name (params->data[0].of.f32, params->data[1].of.f32); \
        return NULL; \
    }

#define ONYX_GL_FLOAT_4(name)                   \
    ONYX_DEF(name, (FLOAT, FLOAT, FLOAT, FLOAT), ()) {  \
        name (params->data[0].of.f32, params->data[1].of.f32, params->data[2].of.f32, params->data[3].of.f32); \
        return NULL; \
    }

#define ONYX_GL_INT_PTR(name, ptr_type)         \
    ONYX_DEF(name, (INT, PTR), ()) {            \
        name (params->data[0].of.i32, (ptr_type *) ONYX_PTR(params->data[1].of.i32)); \
        return NULL; \
    }

#define ONYX_GL_INT_2_PTR(name, ptr_type)       \
    ONYX_DEF(name, (INT, INT, PTR), ()) {       \
        name (params->data[0].of.i32, params->data[1].of.i32, (ptr_type *) ONYX_PTR(params->data[2].of.i32)); \
        return NULL; \
    }


ONYX_GL_0(glFinish)
ONYX_GL_0(glFlush)
ONYX_GL_0(glReleaseShaderCompiler)
ONYX_GL_0(glEndTransformFeedback)
ONYX_GL_0(glPauseTransformFeedback)
ONYX_GL_0(glResumeTransformFeedback)
ONYX_GL_0_RET_INT(glCreateProgram)
ONYX_GL_0_RET_INT(glGetError)
ONYX_GL_INT_1(glActiveTexture)
ONYX_GL_INT_1(glClear)
ONYX_GL_INT_1(glEnable)
ONYX_GL_INT_1(glDisable)
ONYX_GL_INT_1(glBlendEquation)
ONYX_GL_INT_1(glClearStencil)
ONYX_GL_INT_1(glCompileShader)
ONYX_GL_INT_1(glCullFace)
ONYX_GL_INT_1(glDeleteProgram)
ONYX_GL_INT_1(glDeleteShader)
ONYX_GL_INT_1(glDepthFunc)
ONYX_GL_INT_1(glDepthMask)
ONYX_GL_INT_1(glDisableVertexAttribArray)
ONYX_GL_INT_1(glEnableVertexAttribArray)
ONYX_GL_INT_1(glFrontFace)
ONYX_GL_INT_1(glGenerateMipmap)
ONYX_GL_INT_1(glLineWidth)
ONYX_GL_INT_1(glLinkProgram)
ONYX_GL_INT_1(glStencilMask)
ONYX_GL_INT_1(glUseProgram)
ONYX_GL_INT_1(glValidateProgram)
ONYX_GL_INT_1(glReadBuffer)
ONYX_GL_INT_1(glEndQuery)
ONYX_GL_INT_1(glBindVertexArray)
ONYX_GL_INT_1(glBeginTransformFeedback)
ONYX_GL_INT_2(glAttachShader)
ONYX_GL_INT_2(glBindBuffer)
ONYX_GL_INT_2(glBindFramebuffer)
ONYX_GL_INT_2(glBindRenderbuffer)
ONYX_GL_INT_2(glBindTexture)
ONYX_GL_INT_2(glBlendEquationSeparate)
ONYX_GL_INT_2(glBlendFunc)
ONYX_GL_INT_2(glDetachShader)
ONYX_GL_INT_2(glHint)
ONYX_GL_INT_2(glPixelStorei)
ONYX_GL_INT_2(glStencilMaskSeparate)
ONYX_GL_INT_2(glBeginQuery)
ONYX_GL_INT_2(glBindSampler)
ONYX_GL_INT_2(glVertexAttribDivisor)
ONYX_GL_INT_2(glBindTransformFeedback)
ONYX_GL_INT_1_RET_INT(glCheckFramebufferStatus)
ONYX_GL_INT_1_RET_INT(glCreateShader)
ONYX_GL_INT_1_RET_INT(glIsBuffer)
ONYX_GL_INT_1_RET_INT(glIsEnabled)
ONYX_GL_INT_1_RET_INT(glIsFramebuffer)
ONYX_GL_INT_1_RET_INT(glIsProgram )
ONYX_GL_INT_1_RET_INT(glIsRenderbuffer)
ONYX_GL_INT_1_RET_INT(glIsShader)
ONYX_GL_INT_1_RET_INT(glIsTexture)
ONYX_GL_INT_1_RET_INT(glIsQuery)
ONYX_GL_INT_1_RET_INT(glUnmapBuffer)
ONYX_GL_INT_1_RET_INT(glIsVertexArray)
// ONYX_GL_INT_1_RET_INT(glIsSync)
ONYX_GL_INT_1_RET_INT(glIsTransformFeedback)
ONYX_GL_INT_1_RET_INT(glIsSampler)
// ONYX_GL_INT_2_RET_INT(glFenceSync)
ONYX_GL_FLOAT_1(glClearDepthf)
ONYX_GL_FLOAT_2(glDepthRangef)
ONYX_GL_FLOAT_2(glPolygonOffset)
ONYX_GL_FLOAT_4(glClearColor)
ONYX_GL_FLOAT_4(glBlendColor)

ONYX_GL_INT_PTR(glDeleteBuffers, GLuint)
ONYX_GL_INT_PTR(glDeleteFramebuffers, GLuint)
ONYX_GL_INT_PTR(glDeleteRenderbuffers, GLuint)
ONYX_GL_INT_PTR(glDeleteTextures, GLuint)
ONYX_GL_INT_PTR(glGenBuffers, GLuint)
ONYX_GL_INT_PTR(glGenFramebuffers, GLuint)
ONYX_GL_INT_PTR(glGenRenderbuffers, GLuint)
ONYX_GL_INT_PTR(glGenTextures, GLuint)
ONYX_GL_INT_PTR(glGetAttribLocation, GLchar)
ONYX_GL_INT_PTR(glGetBooleanv, GLboolean)
ONYX_GL_INT_PTR(glGetFloatv, GLfloat)
ONYX_GL_INT_PTR(glGetIntegerv, GLint)
ONYX_GL_INT_PTR(glGetUniformLocation, GLchar)
ONYX_GL_INT_PTR(glVertexAttrib1fv, GLfloat)
ONYX_GL_INT_PTR(glVertexAttrib2fv, GLfloat)
ONYX_GL_INT_PTR(glVertexAttrib3fv, GLfloat)
ONYX_GL_INT_PTR(glVertexAttrib4fv, GLfloat)
ONYX_GL_INT_PTR(glGenQueries, GLuint)
ONYX_GL_INT_PTR(glDeleteQueries, GLuint)
ONYX_GL_INT_PTR(glDrawBuffers, GLenum)
ONYX_GL_INT_PTR(glDeleteVertexArrays, GLuint)
ONYX_GL_INT_PTR(glGenVertexArrays, GLuint)
ONYX_GL_INT_PTR(glGenSamplers, GLuint)
ONYX_GL_INT_PTR(glGetInteger64v, GLint64)
ONYX_GL_INT_PTR(glDeleteSamplers, GLuint)
ONYX_GL_INT_PTR(glDeleteTransformFeedbacks, GLuint)
ONYX_GL_INT_PTR(glGenTransformFeedbacks, GLuint)
ONYX_GL_INT_PTR(glGetFragDataLocation, GLchar)
ONYX_GL_INT_PTR(glVertexAttribI4iv, GLint)
ONYX_GL_INT_PTR(glVertexAttribI4uiv, GLuint)
ONYX_GL_INT_3(glDrawArrays)
ONYX_GL_INT_3(glStencilFunc)
ONYX_GL_INT_3(glStencilOp)
ONYX_GL_INT_3(glFlushMappedBufferRange)
ONYX_GL_INT_3(glBindBufferBase)
ONYX_GL_INT_3(glSamplerParameteri)
ONYX_GL_INT_4(glBlendFuncSeparate)
ONYX_GL_INT_4(glColorMask)
ONYX_GL_INT_4(glScissor)
ONYX_GL_INT_4(glStencilFuncSeparate)
ONYX_GL_INT_4(glViewport)
ONYX_GL_INT_4(glMapBufferRange)

ONYX_GL_INT_2_PTR(glBindAttribLocation, GLchar)
ONYX_GL_INT_2_PTR(glGetBufferParameteriv, GLint)
ONYX_GL_INT_2_PTR(glGetProgramiv, GLint)
ONYX_GL_INT_2_PTR(glGetRenderbufferParameteriv, GLint)
ONYX_GL_INT_2_PTR(glGetShaderiv, GLint)
ONYX_GL_INT_2_PTR(glGetTexParameterfv, GLfloat)
ONYX_GL_INT_2_PTR(glGetTexParameteriv, GLint)
ONYX_GL_INT_2_PTR(glGetUniformfv, GLfloat)
ONYX_GL_INT_2_PTR(glGetUniformiv, GLint)
ONYX_GL_INT_2_PTR(glGetVertexAttribfv, GLfloat)
ONYX_GL_INT_2_PTR(glGetVertexAttribiv, GLint)
ONYX_GL_INT_2_PTR(glTexParameterfv, GLfloat)
ONYX_GL_INT_2_PTR(glTexParameteriv, GLint)
ONYX_GL_INT_2_PTR(glUniform1fv, GLfloat)
ONYX_GL_INT_2_PTR(glUniform1iv, GLint)
ONYX_GL_INT_2_PTR(glUniform2fv, GLfloat)
ONYX_GL_INT_2_PTR(glUniform2iv, GLint)
ONYX_GL_INT_2_PTR(glUniform3fv, GLfloat)
ONYX_GL_INT_2_PTR(glUniform3iv, GLint)
ONYX_GL_INT_2_PTR(glUniform4fv, GLfloat)
ONYX_GL_INT_2_PTR(glUniform4iv, GLint)
ONYX_GL_INT_2_PTR(glGetQueryiv, GLint)
ONYX_GL_INT_2_PTR(glGetQueryObjectuiv, GLuint)
ONYX_GL_INT_2_PTR(glGetInteger64i_v, GLint64)
ONYX_GL_INT_2_PTR(glGetBufferParameteri64v, GLint64)
ONYX_GL_INT_2_PTR(glSamplerParameteriv, GLint)
ONYX_GL_INT_2_PTR(glSamplerParameterfv, GLfloat)
ONYX_GL_INT_2_PTR(glGetSamplerParameteriv, GLint)
ONYX_GL_INT_2_PTR(glGetSamplerParameterfv, GLfloat)

// glSamplerParameterf :: (sampler: GLuint, pname: GLenum, param: GLfloat) -> void ---

// glGetBufferPointerv :: (target: GLenum, pname: GLenum, params: ^rawptr) -> void ---
// glGetVertexAttribPointerv :: (index: GLuint, pname: GLenum, pointer: ^rawptr) -> void ---

// glBufferData :: (target: GLenum, size: GLsizeiptr, data: rawptr, usage: GLenum) -> void ---
// glBufferSubData :: (target: GLenum, offset: GLintptr, size: GLsizeiptr, data: rawptr) -> void ---
// glCompressedTexImage2D :: (target: GLenum, level: GLint, internalformat: GLenum, width: GLsizei, height: GLsizei, border: GLint, imageSize: GLsizei, data: rawptr) -> void ---
// glCompressedTexSubImage2D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, width: GLsizei, height: GLsizei, format: GLenum, imageSize: GLsizei, data: rawptr) -> void ---
// glCopyTexImage2D :: (target: GLenum, level: GLint, internalformat: GLenum, x: GLint, y: GLint, width: GLsizei, height: GLsizei, border: GLint) -> void ---
// glCopyTexSubImage2D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> void ---
// glDrawElements :: (mode: GLenum, count: GLsizei, type: GLenum, indices: rawptr) -> void ---
// glFramebufferRenderbuffer :: (target: GLenum, attachment: GLenum, renderbuffertarget: GLenum, renderbuffer: GLuint) -> void ---
// glFramebufferTexture2D :: (target: GLenum, attachment: GLenum, textarget: GLenum, texture: GLuint, level: GLint) -> void ---
// glGetActiveAttrib :: (program: GLuint, index: GLuint, bufSize: GLsizei, length: ^GLsizei, size: ^GLint, type: ^GLenum, name: ^GLchar) -> void ---
// glGetActiveUniform :: (program: GLuint, index: GLuint, bufSize: GLsizei, length: ^GLsizei, size: ^GLint, type: ^GLenum, name: ^GLchar) -> void ---
// glGetAttachedShaders :: (program: GLuint, maxCount: GLsizei, count: ^GLsizei, shaders: ^GLuint) -> void ---
// glGetFramebufferAttachmentParameteriv :: (target: GLenum, attachment: GLenum, pname: GLenum, params: ^GLint) -> void ---
// glGetProgramInfoLog :: (program: GLuint, bufSize: GLsizei, length: ^GLsizei, infoLog: ^GLchar) -> void ---
// glGetShaderInfoLog :: (shader: GLuint, bufSize: GLsizei, length: ^GLsizei, infoLog: ^GLchar) -> void ---
// glGetShaderPrecisionFormat :: (shadertype: GLenum, precisiontype: GLenum, range: ^GLint, precision: ^GLint) -> void ---
// glGetShaderSource :: (shader: GLuint, bufSize: GLsizei, length: ^GLsizei, source: ^GLchar) -> void ---
// glGetString :: (name: GLenum) -> ^GLubyte ---
// glReadPixels :: (x: GLint, y: GLint, width: GLsizei, height: GLsizei, format: GLenum, type: GLenum, pixels: rawptr) -> void ---
// glRenderbufferStorage :: (target: GLenum, internalformat: GLenum, width: GLsizei, height: GLsizei) -> void ---
// glSampleCoverage :: (value: GLfloat, invert: GLboolean) -> void ---
// glShaderBinary :: (count: GLsizei, shaders: ^GLuint, binaryformat: GLenum, binary: rawptr, length: GLsizei) -> void ---
// glShaderSource :: (shader: GLuint, count: GLsizei, conststring: ^^GLchar, length: ^GLint) -> void ---
// glStencilOpSeparate :: (face: GLenum, sfail: GLenum, dpfail: GLenum, dppass: GLenum) -> void ---
// glTexImage2D :: (target: GLenum, level: GLint, internalformat: GLint, width: GLsizei, height: GLsizei, border: GLint, format: GLenum, type: GLenum, pixels: rawptr) -> void ---
// glTexParameterf :: (target: GLenum, pname: GLenum, param: GLfloat) -> void ---
// glTexParameteri :: (target: GLenum, pname: GLenum, param: GLint) -> void ---
// glTexSubImage2D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, width: GLsizei, height: GLsizei, format: GLenum, type: GLenum, pixels: rawptr) -> void ---
// glUniform1f :: (location: GLint, v0: GLfloat) -> void ---
// glUniform1i :: (location: GLint, v0: GLint) -> void ---
// glUniform2f :: (location: GLint, v0: GLfloat, v1: GLfloat) -> void ---
// glUniform2i :: (location: GLint, v0: GLint, v1: GLint) -> void ---
// glUniform3f :: (location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat) -> void ---
// glUniform3i :: (location: GLint, v0: GLint, v1: GLint, v2: GLint) -> void ---
// glUniform4f :: (location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat, v3: GLfloat) -> void ---
// glUniform4i :: (location: GLint, v0: GLint, v1: GLint, v2: GLint, v3: GLint) -> void ---
// glUniformMatrix2fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix3fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix4fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glVertexAttrib1f :: (index: GLuint, x: GLfloat) -> void ---
// glVertexAttrib2f :: (index: GLuint, x: GLfloat, y: GLfloat) -> void ---
// glVertexAttrib3f :: (index: GLuint, x: GLfloat, y: GLfloat, z: GLfloat) -> void ---
// glVertexAttrib4f :: (index: GLuint, x: GLfloat, y: GLfloat, z: GLfloat, w: GLfloat) -> void ---
// glVertexAttribPointer :: (index: GLuint, size: GLint, type: GLenum, normalized: GLboolean, stride: GLsizei, pointer: rawptr) -> void ---

// glGetIntegeri_v :: (target: GLenum, index: GLuint, data: ^GLint) -> void ---
// glGetVertexAttribIiv :: (index: GLuint, pname: GLenum, params: ^GLint) -> void ---
// glGetVertexAttribIuiv :: (index: GLuint, pname: GLenum, params: ^GLuint) -> void ---
// glGetUniformuiv :: (program: GLuint, location: GLint, params: ^GLuint) -> void ---
// glUniform1uiv :: (location: GLint, count: GLsizei, value: ^GLuint) -> void ---
// glUniform2uiv :: (location: GLint, count: GLsizei, value: ^GLuint) -> void ---
// glUniform3uiv :: (location: GLint, count: GLsizei, value: ^GLuint) -> void ---
// glUniform4uiv :: (location: GLint, count: GLsizei, value: ^GLuint) -> void ---
// glClearBufferiv :: (buffer: GLenum, drawbuffer: GLint, value: ^GLint) -> void ---
// glClearBufferuiv :: (buffer: GLenum, drawbuffer: GLint, value: ^GLuint) -> void ---
// glClearBufferfv :: (buffer: GLenum, drawbuffer: GLint, value: ^GLfloat) -> void ---
// Open3: GLES
// glDrawRangeElements :: (mode: GLenum, start: GLuint, end: GLuint, count: GLsizei, type: GLenum, indices: rawptr) -> void ---
// glTexImage3D :: (target: GLenum, level: GLint, internalformat: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, border: GLint, format: GLenum, type: GLenum, pixels: rawptr) -> void ---
// glTexSubImage3D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, format: GLenum, type: GLenum, pixels: rawptr) -> void ---
// glCopyTexSubImage3D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> void ---
// glCompressedTexImage3D :: (target: GLenum, level: GLint, internalformat: GLenum, width: GLsizei, height: GLsizei, depth: GLsizei, border: GLint, imageSize: GLsizei, data: rawptr) -> void ---
// glCompressedTexSubImage3D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, format: GLenum, imageSize: GLsizei, data: rawptr) -> void ---
// glUniformMatrix2x3fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix3x2fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix2x4fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix4x2fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix3x4fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix4x3fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glBlitFramebuffer :: (srcX0: GLint, srcY0: GLint, srcX1: GLint, srcY1: GLint, dstX0: GLint, dstY0: GLint, dstX1: GLint, dstY1: GLint, mask: GLbitfield, filter: GLenum) -> void ---
// glRenderbufferStorageMultisample :: (target: GLenum, samples: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei) -> void ---
// glFramebufferTextureLayer :: (target: GLenum, attachment: GLenum, texture: GLuint, level: GLint, layer: GLint) -> void ---
// glBindBufferRange :: (target: GLenum, index: GLuint, buffer: GLuint, offset: GLintptr, size: GLsizeiptr) -> void ---
// glTransformFeedbackVaryings :: (program: GLuint, count: GLsizei, varyings: ^^GLchar, bufferMode: GLenum) -> void ---
// glGetTransformFeedbackVarying :: (program: GLuint, index: GLuint, bufSize: GLsizei, length: ^GLsizei, size: ^GLsizei, type: ^GLenum, name: ^GLchar) -> void ---
// glVertexAttribIPointer :: (index: GLuint, size: GLint, type: GLenum, stride: GLsizei, pointer: rawptr) -> void ---
// glVertexAttribI4i :: (index: GLuint, x: GLint, y: GLint, z: GLint, w: GLint) -> void ---
// glVertexAttribI4ui :: (index: GLuint, x: GLuint, y: GLuint, z: GLuint, w: GLuint) -> void ---
// glUniform1ui :: (location: GLint, v0: GLuint) -> void ---
// glUniform2ui :: (location: GLint, v0: GLuint, v1: GLuint) -> void ---
// glUniform3ui :: (location: GLint, v0: GLuint, v1: GLuint, v2: GLuint) -> void ---
// glUniform4ui :: (location: GLint, v0: GLuint, v1: GLuint, v2: GLuint, v3: GLuint) -> void ---
// glClearBufferfi :: (buffer: GLenum, drawbuffer: GLint, depth: GLfloat, stencil: GLint) -> void ---
// glGetStringi :: (name: GLenum, index: GLuint) -> ^GLubyte ---
// glCopyBufferSubData :: (readTarget: GLenum, writeTarget: GLenum, readOffset: GLintptr, writeOffset: GLintptr, size: GLsizeiptr) -> void ---
// glGetUniformIndices :: (program: GLuint, uniformCount: GLsizei, uniformNames: ^^GLchar, uniformIndices: ^GLuint) -> void ---
// glGetActiveUniformsiv :: (program: GLuint, uniformCount: GLsizei, uniformIndices: ^GLuint, pname: GLenum, params: ^GLint) -> void ---
// glGetUniformBlockIndex :: (program: GLuint, uniformBlockName: ^GLchar) -> GLuint ---
// glGetActiveUniformBlockiv :: (program: GLuint, uniformBlockIndex: GLuint, pname: GLenum, params: ^GLint) -> void ---
// glGetActiveUniformBlockName :: (program: GLuint, uniformBlockIndex: GLuint, bufSize: GLsizei, length: ^GLsizei, uniformBlockName: ^GLchar) -> void ---
// glUniformBlockBinding :: (program: GLuint, uniformBlockIndex: GLuint, uniformBlockBinding: GLuint) -> void ---
// glDrawArraysInstanced :: (mode: GLenum, first: GLint, count: GLsizei, instancecount: GLsizei) -> void ---
// glDrawElementsInstanced :: (mode: GLenum, count: GLsizei, type: GLenum, indices: rawptr, instancecount: GLsizei) -> void ---
// glClientWaitSync :: (sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> GLenum ---
// glWaitSync :: (sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> void ---
// glGetSynciv :: (sync: GLsync, pname: GLenum, bufSize: GLsizei, length: ^GLsizei, values: ^GLint) -> void ---
// glGetProgramBinary :: (program: GLuint, bufSize: GLsizei, length: ^GLsizei, binaryFormat: ^GLenum, binary: rawptr) -> void ---
// glProgramBinary :: (program: GLuint, binaryFormat: GLenum, binary: rawptr, length: GLsizei) -> void ---
// glProgramParameteri :: (program: GLuint, pname: GLenum, value: GLint) -> void ---
// glInvalidateFramebuffer :: (target: GLenum, numAttachments: GLsizei, attachments: ^GLenum) -> void ---
// glInvalidateSubFramebuffer :: (target: GLenum, numAttachments: GLsizei, attachments: ^GLenum, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> void ---
// glTexStorage2D :: (target: GLenum, levels: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei) -> void ---
// glTexStorage3D :: (target: GLenum, levels: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei, depth: GLsizei) -> void ---
// glGetInternalformativ :: (target: GLenum, internalformat: GLenum, pname: GLenum, bufSize: GLsizei, params: ^GLint) -> void ---

ONYX_LIBRARY {
    ONYX_FUNC(glClearColor)
    ONYX_FUNC(glFinish)
    ONYX_FUNC(glFlush)
    ONYX_FUNC(glReleaseShaderCompiler)
    ONYX_FUNC(glEndTransformFeedback)
    ONYX_FUNC(glPauseTransformFeedback)
    ONYX_FUNC(glResumeTransformFeedback)
    ONYX_FUNC(glActiveTexture)
    ONYX_FUNC(glClear)
    ONYX_FUNC(glEnable)
    ONYX_FUNC(glDisable)
    ONYX_FUNC(glBlendEquation)
    ONYX_FUNC(glClearStencil)
    ONYX_FUNC(glCompileShader)
    ONYX_FUNC(glCullFace)
    ONYX_FUNC(glDeleteProgram)
    ONYX_FUNC(glDeleteShader)
    ONYX_FUNC(glDepthFunc)
    ONYX_FUNC(glDepthMask)
    ONYX_FUNC(glDisableVertexAttribArray)
    ONYX_FUNC(glEnableVertexAttribArray)
    ONYX_FUNC(glFrontFace)
    ONYX_FUNC(glGenerateMipmap)
    ONYX_FUNC(glLineWidth)
    ONYX_FUNC(glLinkProgram)
    ONYX_FUNC(glStencilMask)
    ONYX_FUNC(glUseProgram)
    ONYX_FUNC(glValidateProgram)
    ONYX_FUNC(glReadBuffer)
    ONYX_FUNC(glEndQuery)
    ONYX_FUNC(glBindVertexArray)
    ONYX_FUNC(glBeginTransformFeedback)
    ONYX_FUNC(glAttachShader)
    ONYX_FUNC(glBindBuffer)
    ONYX_FUNC(glBindFramebuffer)
    ONYX_FUNC(glBindRenderbuffer)
    ONYX_FUNC(glBindTexture)
    ONYX_FUNC(glCreateProgram)
    ONYX_FUNC(glGetError)
    ONYX_FUNC(glBlendEquationSeparate)
    ONYX_FUNC(glBlendFunc)
    ONYX_FUNC(glDetachShader)
    ONYX_FUNC(glHint)
    ONYX_FUNC(glPixelStorei)
    ONYX_FUNC(glStencilMaskSeparate)
    ONYX_FUNC(glBeginQuery)
    ONYX_FUNC(glBindSampler)
    ONYX_FUNC(glVertexAttribDivisor)
    ONYX_FUNC(glBindTransformFeedback)
    ONYX_FUNC(glCheckFramebufferStatus)
    ONYX_FUNC(glCreateShader)
    ONYX_FUNC(glIsBuffer)
    ONYX_FUNC(glIsEnabled)
    ONYX_FUNC(glIsFramebuffer)
    ONYX_FUNC(glIsProgram )
    ONYX_FUNC(glIsRenderbuffer)
    ONYX_FUNC(glIsShader)
    ONYX_FUNC(glIsTexture)
    ONYX_FUNC(glIsQuery)
    ONYX_FUNC(glUnmapBuffer)
    ONYX_FUNC(glIsVertexArray)
    ONYX_FUNC(glIsTransformFeedback)
    ONYX_FUNC(glIsSampler)
    // ONYX_FUNC(glFenceSync)
    ONYX_FUNC(glClearDepthf)
    ONYX_FUNC(glDepthRangef)
    ONYX_FUNC(glPolygonOffset)
    ONYX_FUNC(glClearColor)
    ONYX_FUNC(glBlendColor)

    ONYX_FUNC(glDeleteBuffers)
    ONYX_FUNC(glDeleteFramebuffers)
    ONYX_FUNC(glDeleteRenderbuffers)
    ONYX_FUNC(glDeleteTextures)
    ONYX_FUNC(glGenBuffers)
    ONYX_FUNC(glGenFramebuffers)
    ONYX_FUNC(glGenRenderbuffers)
    ONYX_FUNC(glGenTextures)
    ONYX_FUNC(glGetAttribLocation)
    ONYX_FUNC(glGetBooleanv)
    ONYX_FUNC(glGetFloatv)
    ONYX_FUNC(glGetIntegerv)
    ONYX_FUNC(glGetUniformLocation)
    ONYX_FUNC(glVertexAttrib1fv)
    ONYX_FUNC(glVertexAttrib2fv)
    ONYX_FUNC(glVertexAttrib3fv)
    ONYX_FUNC(glVertexAttrib4fv)
    ONYX_FUNC(glGenQueries)
    ONYX_FUNC(glDeleteQueries)
    ONYX_FUNC(glDrawBuffers)
    ONYX_FUNC(glDeleteVertexArrays)
    ONYX_FUNC(glGenVertexArrays)
    ONYX_FUNC(glGenSamplers)
    ONYX_FUNC(glGetInteger64v)
    ONYX_FUNC(glDeleteSamplers)
    ONYX_FUNC(glDeleteTransformFeedbacks)
    ONYX_FUNC(glGenTransformFeedbacks)

    NULL
};
