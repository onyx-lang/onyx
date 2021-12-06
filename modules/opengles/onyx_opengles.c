#include "onyx_library.h"
#include <GLES3/gl3.h>

#define ONYX_LIBRARY_NAME onyx_opengles

#define ONYX_GL_0(name) \
    ONYX_DEF(name, (), ()) { \
        name (); \
        return NULL;                   \
    }

#define ONYX_GL_INT_1(name) \
    ONYX_DEF(name, (INT), ()) { \
        name (params->data[0].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_2(name) \
    ONYX_DEF(name, (INT, INT), ()) { \
        name (params->data[0].of.i32, params->data[0].of.i32); \
        return NULL;                   \
    }

ONYX_GL_0(glFinish)
ONYX_GL_0(glFlush)
ONYX_GL_0(glReleaseShaderCompiler)
ONYX_GL_0(glEndTransformFeedback)
ONYX_GL_0(glPauseTransformFeedback)
ONYX_GL_0(glResumeTransformFeedback)
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
// glBlendEquationSeparate :: (modeRGB: GLenum, modeAlpha: GLenum) -> void ---
// glBlendFunc :: (sfactor: GLenum, dfactor: GLenum) -> void ---
// glDetachShader :: (program: GLuint, shader: GLuint) -> void ---
// glHint :: (target: GLenum, mode: GLenum) -> void ---
// glPixelStorei :: (pname: GLenum, param: GLint) -> void ---
// glStencilMaskSeparate :: (face: GLenum, mask: GLuint) -> void ---

ONYX_DEF(glClearColor, (FLOAT, FLOAT, FLOAT, FLOAT), ()) {
    glClearColor(
        params->data[0].of.f32,
        params->data[1].of.f32,
        params->data[2].of.f32,
        params->data[3].of.f32);
    return NULL;
}

// glBindAttribLocation :: (program: GLuint, index: GLuint, name: ^GLchar) -> void ---
// glBlendColor :: (red: GLfloat, green: GLfloat, blue: GLfloat, alpha: GLfloat) -> void ---
// glBlendFuncSeparate :: (sfactorRGB: GLenum, dfactorRGB: GLenum, sfactorAlpha: GLenum, dfactorAlpha: GLenum) -> void ---
// glBufferData :: (target: GLenum, size: GLsizeiptr, data: rawptr, usage: GLenum) -> void ---
// glBufferSubData :: (target: GLenum, offset: GLintptr, size: GLsizeiptr, data: rawptr) -> void ---
// glCheckFramebufferStatus :: (target: GLenum) -> GLenum ---
// glClearDepthf :: (d: GLfloat) -> void ---
// glColorMask :: (red: GLboolean, green: GLboolean, blue: GLboolean, alpha: GLboolean) -> void ---
// glCompressedTexImage2D :: (target: GLenum, level: GLint, internalformat: GLenum, width: GLsizei, height: GLsizei, border: GLint, imageSize: GLsizei, data: rawptr) -> void ---
// glCompressedTexSubImage2D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, width: GLsizei, height: GLsizei, format: GLenum, imageSize: GLsizei, data: rawptr) -> void ---
// glCopyTexImage2D :: (target: GLenum, level: GLint, internalformat: GLenum, x: GLint, y: GLint, width: GLsizei, height: GLsizei, border: GLint) -> void ---
// glCopyTexSubImage2D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> void ---
// glCreateProgram :: () -> GLuint ---
// glCreateShader :: (type: GLenum) -> GLuint ---
// glDeleteBuffers :: (n: GLsizei, buffers: ^GLuint) -> void ---
// glDeleteFramebuffers :: (n: GLsizei, framebuffers: ^GLuint) -> void ---
// glDeleteRenderbuffers :: (n: GLsizei, renderbuffers: ^GLuint) -> void ---
// glDeleteTextures :: (n: GLsizei, textures: ^GLuint) -> void ---
// glDepthRangef :: (n: GLfloat, f: GLfloat) -> void ---
// glDrawArrays :: (mode: GLenum, first: GLint, count: GLsizei) -> void ---
// glDrawElements :: (mode: GLenum, count: GLsizei, type: GLenum, indices: rawptr) -> void ---
// glFramebufferRenderbuffer :: (target: GLenum, attachment: GLenum, renderbuffertarget: GLenum, renderbuffer: GLuint) -> void ---
// glFramebufferTexture2D :: (target: GLenum, attachment: GLenum, textarget: GLenum, texture: GLuint, level: GLint) -> void ---
// glGenBuffers :: (n: GLsizei, buffers: ^GLuint) -> void ---
// glGenFramebuffers :: (n: GLsizei, framebuffers: ^GLuint) -> void ---
// glGenRenderbuffers :: (n: GLsizei, renderbuffers: ^GLuint) -> void ---
// glGenTextures :: (n: GLsizei, textures: ^GLuint) -> void ---
// glGetActiveAttrib :: (program: GLuint, index: GLuint, bufSize: GLsizei, length: ^GLsizei, size: ^GLint, type: ^GLenum, name: ^GLchar) -> void ---
// glGetActiveUniform :: (program: GLuint, index: GLuint, bufSize: GLsizei, length: ^GLsizei, size: ^GLint, type: ^GLenum, name: ^GLchar) -> void ---
// glGetAttachedShaders :: (program: GLuint, maxCount: GLsizei, count: ^GLsizei, shaders: ^GLuint) -> void ---
// glGetAttribLocation :: (program: GLuint, name: ^GLchar) -> GLint ---
// glGetBooleanv :: (pname: GLenum, data: ^GLboolean) -> void ---
// glGetBufferParameteriv :: (target: GLenum, pname: GLenum, params: ^GLint) -> void ---
// glGetError :: () -> GLenum ---
// glGetFloatv :: (pname: GLenum, data: ^GLfloat) -> void ---
// glGetFramebufferAttachmentParameteriv :: (target: GLenum, attachment: GLenum, pname: GLenum, params: ^GLint) -> void ---
// glGetIntegerv :: (pname: GLenum, data: ^GLint) -> void ---
// glGetProgramiv :: (program: GLuint, pname: GLenum, params: ^GLint) -> void ---
// glGetProgramInfoLog :: (program: GLuint, bufSize: GLsizei, length: ^GLsizei, infoLog: ^GLchar) -> void ---
// glGetRenderbufferParameteriv :: (target: GLenum, pname: GLenum, params: ^GLint) -> void ---
// glGetShaderiv :: (shader: GLuint, pname: GLenum, params: ^GLint) -> void ---
// glGetShaderInfoLog :: (shader: GLuint, bufSize: GLsizei, length: ^GLsizei, infoLog: ^GLchar) -> void ---
// glGetShaderPrecisionFormat :: (shadertype: GLenum, precisiontype: GLenum, range: ^GLint, precision: ^GLint) -> void ---
// glGetShaderSource :: (shader: GLuint, bufSize: GLsizei, length: ^GLsizei, source: ^GLchar) -> void ---
// glGetString :: (name: GLenum) -> ^GLubyte ---
// glGetTexParameterfv :: (target: GLenum, pname: GLenum, params: ^GLfloat) -> void ---
// glGetTexParameteriv :: (target: GLenum, pname: GLenum, params: ^GLint) -> void ---
// glGetUniformfv :: (program: GLuint, location: GLint, params: ^GLfloat) -> void ---
// glGetUniformiv :: (program: GLuint, location: GLint, params: ^GLint) -> void ---
// glGetUniformLocation :: (program: GLuint, name: ^GLchar) -> GLint ---
// glGetVertexAttribfv :: (index: GLuint, pname: GLenum, params: ^GLfloat) -> void ---
// glGetVertexAttribiv :: (index: GLuint, pname: GLenum, params: ^GLint) -> void ---
// glGetVertexAttribPointerv :: (index: GLuint, pname: GLenum, pointer: ^rawptr) -> void ---
// glIsBuffer :: (buffer: GLuint) -> GLboolean ---
// glIsEnabled :: (cap: GLenum) -> GLboolean ---
// glIsFramebuffer :: (framebuffer: GLuint) -> GLboolean ---
// glIsProgram :: (program: GLuint) -> GLboolean ---
// glIsRenderbuffer :: (renderbuffer: GLuint) -> GLboolean ---
// glIsShader :: (shader: GLuint) -> GLboolean ---
// glIsTexture :: (texture: GLuint) -> GLboolean ---
// glPolygonOffset :: (factor: GLfloat, units: GLfloat) -> void ---
// glReadPixels :: (x: GLint, y: GLint, width: GLsizei, height: GLsizei, format: GLenum, type: GLenum, pixels: rawptr) -> void ---
// glRenderbufferStorage :: (target: GLenum, internalformat: GLenum, width: GLsizei, height: GLsizei) -> void ---
// glSampleCoverage :: (value: GLfloat, invert: GLboolean) -> void ---
// glScissor :: (x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> void ---
// glShaderBinary :: (count: GLsizei, shaders: ^GLuint, binaryformat: GLenum, binary: rawptr, length: GLsizei) -> void ---
// glShaderSource :: (shader: GLuint, count: GLsizei, conststring: ^^GLchar, length: ^GLint) -> void ---
// glStencilFunc :: (func: GLenum, ref: GLint, mask: GLuint) -> void ---
// glStencilFuncSeparate :: (face: GLenum, func: GLenum, ref: GLint, mask: GLuint) -> void ---
// glStencilOp :: (fail: GLenum, zfail: GLenum, zpass: GLenum) -> void ---
// glStencilOpSeparate :: (face: GLenum, sfail: GLenum, dpfail: GLenum, dppass: GLenum) -> void ---
// glTexImage2D :: (target: GLenum, level: GLint, internalformat: GLint, width: GLsizei, height: GLsizei, border: GLint, format: GLenum, type: GLenum, pixels: rawptr) -> void ---
// glTexParameterf :: (target: GLenum, pname: GLenum, param: GLfloat) -> void ---
// glTexParameterfv :: (target: GLenum, pname: GLenum, params: ^GLfloat) -> void ---
// glTexParameteri :: (target: GLenum, pname: GLenum, param: GLint) -> void ---
// glTexParameteriv :: (target: GLenum, pname: GLenum, params: ^GLint) -> void ---
// glTexSubImage2D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, width: GLsizei, height: GLsizei, format: GLenum, type: GLenum, pixels: rawptr) -> void ---
// glUniform1f :: (location: GLint, v0: GLfloat) -> void ---
// glUniform1fv :: (location: GLint, count: GLsizei, value: ^GLfloat) -> void ---
// glUniform1i :: (location: GLint, v0: GLint) -> void ---
// glUniform1iv :: (location: GLint, count: GLsizei, value: ^GLint) -> void ---
// glUniform2f :: (location: GLint, v0: GLfloat, v1: GLfloat) -> void ---
// glUniform2fv :: (location: GLint, count: GLsizei, value: ^GLfloat) -> void ---
// glUniform2i :: (location: GLint, v0: GLint, v1: GLint) -> void ---
// glUniform2iv :: (location: GLint, count: GLsizei, value: ^GLint) -> void ---
// glUniform3f :: (location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat) -> void ---
// glUniform3fv :: (location: GLint, count: GLsizei, value: ^GLfloat) -> void ---
// glUniform3i :: (location: GLint, v0: GLint, v1: GLint, v2: GLint) -> void ---
// glUniform3iv :: (location: GLint, count: GLsizei, value: ^GLint) -> void ---
// glUniform4f :: (location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat, v3: GLfloat) -> void ---
// glUniform4fv :: (location: GLint, count: GLsizei, value: ^GLfloat) -> void ---
// glUniform4i :: (location: GLint, v0: GLint, v1: GLint, v2: GLint, v3: GLint) -> void ---
// glUniform4iv :: (location: GLint, count: GLsizei, value: ^GLint) -> void ---
// glUniformMatrix2fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix3fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix4fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glVertexAttrib1f :: (index: GLuint, x: GLfloat) -> void ---
// glVertexAttrib1fv :: (index: GLuint, v: ^GLfloat) -> void ---
// glVertexAttrib2f :: (index: GLuint, x: GLfloat, y: GLfloat) -> void ---
// glVertexAttrib2fv :: (index: GLuint, v: ^GLfloat) -> void ---
// glVertexAttrib3f :: (index: GLuint, x: GLfloat, y: GLfloat, z: GLfloat) -> void ---
// glVertexAttrib3fv :: (index: GLuint, v: ^GLfloat) -> void ---
// glVertexAttrib4f :: (index: GLuint, x: GLfloat, y: GLfloat, z: GLfloat, w: GLfloat) -> void ---
// glVertexAttrib4fv :: (index: GLuint, v: ^GLfloat) -> void ---
// glVertexAttribPointer :: (index: GLuint, size: GLint, type: GLenum, normalized: GLboolean, stride: GLsizei, pointer: rawptr) -> void ---
// glViewport :: (x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> void ---


// Open3: GLES
// glDrawRangeElements :: (mode: GLenum, start: GLuint, end: GLuint, count: GLsizei, type: GLenum, indices: rawptr) -> void ---
// glTexImage3D :: (target: GLenum, level: GLint, internalformat: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, border: GLint, format: GLenum, type: GLenum, pixels: rawptr) -> void ---
// glTexSubImage3D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, format: GLenum, type: GLenum, pixels: rawptr) -> void ---
// glCopyTexSubImage3D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> void ---
// glCompressedTexImage3D :: (target: GLenum, level: GLint, internalformat: GLenum, width: GLsizei, height: GLsizei, depth: GLsizei, border: GLint, imageSize: GLsizei, data: rawptr) -> void ---
// glCompressedTexSubImage3D :: (target: GLenum, level: GLint, xoffset: GLint, yoffset: GLint, zoffset: GLint, width: GLsizei, height: GLsizei, depth: GLsizei, format: GLenum, imageSize: GLsizei, data: rawptr) -> void ---
// glGenQueries :: (n: GLsizei, ids: ^GLuint) -> void ---
// glDeleteQueries :: (n: GLsizei, ids: ^GLuint) -> void ---
// glIsQuery :: (id: GLuint) -> GLboolean ---
// glBeginQuery :: (target: GLenum, id: GLuint) -> void ---
// glGetQueryiv :: (target: GLenum, pname: GLenum, params: ^GLint) -> void ---
// glGetQueryObjectuiv :: (id: GLuint, pname: GLenum, params: ^GLuint) -> void ---
// glUnmapBuffer :: (target: GLenum) -> GLboolean ---
// glGetBufferPointerv :: (target: GLenum, pname: GLenum, params: ^rawptr) -> void ---
// glDrawBuffers :: (n: GLsizei, bufs: ^GLenum) -> void ---
// glUniformMatrix2x3fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix3x2fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix2x4fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix4x2fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix3x4fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glUniformMatrix4x3fv :: (location: GLint, count: GLsizei, transpose: GLboolean, value: ^GLfloat) -> void ---
// glBlitFramebuffer :: (srcX0: GLint, srcY0: GLint, srcX1: GLint, srcY1: GLint, dstX0: GLint, dstY0: GLint, dstX1: GLint, dstY1: GLint, mask: GLbitfield, filter: GLenum) -> void ---
// glRenderbufferStorageMultisample :: (target: GLenum, samples: GLsizei, internalformat: GLenum, width: GLsizei, height: GLsizei) -> void ---
// glFramebufferTextureLayer :: (target: GLenum, attachment: GLenum, texture: GLuint, level: GLint, layer: GLint) -> void ---
// glMapBufferRange :: (target: GLenum, offset: GLintptr, length: GLsizeiptr, access: GLbitfield) -> rawptr ---
// glFlushMappedBufferRange :: (target: GLenum, offset: GLintptr, length: GLsizeiptr) -> void ---
// glDeleteVertexArrays :: (n: GLsizei, arrays: ^GLuint) -> void ---
// glGenVertexArrays :: (n: GLsizei, arrays: ^GLuint) -> void ---
// glIsVertexArray :: (array: GLuint) -> GLboolean ---
// glGetIntegeri_v :: (target: GLenum, index: GLuint, data: ^GLint) -> void ---
// glBindBufferRange :: (target: GLenum, index: GLuint, buffer: GLuint, offset: GLintptr, size: GLsizeiptr) -> void ---
// glBindBufferBase :: (target: GLenum, index: GLuint, buffer: GLuint) -> void ---
// glTransformFeedbackVaryings :: (program: GLuint, count: GLsizei, varyings: ^^GLchar, bufferMode: GLenum) -> void ---
// glGetTransformFeedbackVarying :: (program: GLuint, index: GLuint, bufSize: GLsizei, length: ^GLsizei, size: ^GLsizei, type: ^GLenum, name: ^GLchar) -> void ---
// glVertexAttribIPointer :: (index: GLuint, size: GLint, type: GLenum, stride: GLsizei, pointer: rawptr) -> void ---
// glGetVertexAttribIiv :: (index: GLuint, pname: GLenum, params: ^GLint) -> void ---
// glGetVertexAttribIuiv :: (index: GLuint, pname: GLenum, params: ^GLuint) -> void ---
// glVertexAttribI4i :: (index: GLuint, x: GLint, y: GLint, z: GLint, w: GLint) -> void ---
// glVertexAttribI4ui :: (index: GLuint, x: GLuint, y: GLuint, z: GLuint, w: GLuint) -> void ---
// glVertexAttribI4iv :: (index: GLuint, v: ^GLint) -> void ---
// glVertexAttribI4uiv :: (index: GLuint, v: ^GLuint) -> void ---
// glGetUniformuiv :: (program: GLuint, location: GLint, params: ^GLuint) -> void ---
// glGetFragDataLocation :: (program: GLuint, name: ^GLchar) -> GLint ---
// glUniform1ui :: (location: GLint, v0: GLuint) -> void ---
// glUniform2ui :: (location: GLint, v0: GLuint, v1: GLuint) -> void ---
// glUniform3ui :: (location: GLint, v0: GLuint, v1: GLuint, v2: GLuint) -> void ---
// glUniform4ui :: (location: GLint, v0: GLuint, v1: GLuint, v2: GLuint, v3: GLuint) -> void ---
// glUniform1uiv :: (location: GLint, count: GLsizei, value: ^GLuint) -> void ---
// glUniform2uiv :: (location: GLint, count: GLsizei, value: ^GLuint) -> void ---
// glUniform3uiv :: (location: GLint, count: GLsizei, value: ^GLuint) -> void ---
// glUniform4uiv :: (location: GLint, count: GLsizei, value: ^GLuint) -> void ---
// glClearBufferiv :: (buffer: GLenum, drawbuffer: GLint, value: ^GLint) -> void ---
// glClearBufferuiv :: (buffer: GLenum, drawbuffer: GLint, value: ^GLuint) -> void ---
// glClearBufferfv :: (buffer: GLenum, drawbuffer: GLint, value: ^GLfloat) -> void ---
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
// glFenceSync :: (condition: GLenum, flags: GLbitfield) -> GLsync ---
// glIsSync :: (sync: GLsync) -> GLboolean ---
// glClientWaitSync :: (sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> GLenum ---
// glWaitSync :: (sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> void ---
// glGetInteger64v :: (pname: GLenum, data: ^GLint64) -> void ---
// glGetSynciv :: (sync: GLsync, pname: GLenum, bufSize: GLsizei, length: ^GLsizei, values: ^GLint) -> void ---
// glGetInteger64i_v :: (target: GLenum, index: GLuint, data: ^GLint64) -> void ---
// glGetBufferParameteri64v :: (target: GLenum, pname: GLenum, params: ^GLint64) -> void ---
// glGenSamplers :: (count: GLsizei, samplers: ^GLuint) -> void ---
// glDeleteSamplers :: (count: GLsizei, samplers: ^GLuint) -> void ---
// glIsSampler :: (sampler: GLuint) -> GLboolean ---
// glBindSampler :: (unit: GLuint, sampler: GLuint) -> void ---
// glSamplerParameteri :: (sampler: GLuint, pname: GLenum, param: GLint) -> void ---
// glSamplerParameteriv :: (sampler: GLuint, pname: GLenum, param: ^GLint) -> void ---
// glSamplerParameterf :: (sampler: GLuint, pname: GLenum, param: GLfloat) -> void ---
// glSamplerParameterfv :: (sampler: GLuint, pname: GLenum, param: ^GLfloat) -> void ---
// glGetSamplerParameteriv :: (sampler: GLuint, pname: GLenum, params: ^GLint) -> void ---
// glGetSamplerParameterfv :: (sampler: GLuint, pname: GLenum, params: ^GLfloat) -> void ---
// glVertexAttribDivisor :: (index: GLuint, divisor: GLuint) -> void ---
// glBindTransformFeedback :: (target: GLenum, id: GLuint) -> void ---
// glDeleteTransformFeedbacks :: (n: GLsizei, ids: ^GLuint) -> void ---
// glGenTransformFeedbacks :: (n: GLsizei, ids: ^GLuint) -> void ---
// glIsTransformFeedback :: (id: GLuint) -> GLboolean ---
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

    NULL
};