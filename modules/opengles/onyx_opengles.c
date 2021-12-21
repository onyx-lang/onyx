
#define GLAD_GLES2_IMPLEMENTATION
#include "glad.h"

#define ONYX_LIBRARY_NAME onyx_opengles
#include "onyx_library.h"

#if defined(_WIN32) || defined(_WIN64)
    // Why is Windows dumb?
    #define alloca _alloca
#endif

#define ONYX_GL_0(name) \
    ONYX_DEF(name, (), ()) { \
        glad_##name (); \
        return NULL;                   \
    }

#define ONYX_GL_0_RET_INT(name) \
    ONYX_DEF(name, (), (INT)) { \
        results->data[0] = WASM_I32_VAL( glad_##name () ); \
        return NULL;                   \
    }

#define ONYX_GL_INT_1(name) \
    ONYX_DEF(name, (INT), ()) { \
        glad_##name (params->data[0].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_1_RET_INT(name)             \
    ONYX_DEF(name, (INT), (INT)) {              \
        results->data[0] = WASM_I32_VAL( glad_##name (params->data[0].of.i32) ); \
        return NULL; \
    }

#define ONYX_GL_INT_2(name) \
    ONYX_DEF(name, (INT, INT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_2_RET_INT(name) \
    ONYX_DEF(name, (INT, INT), (INT)) { \
        results->data[0] = WASM_I32_VAL( glad_##name (params->data[0].of.i32, params->data[1].of.i32) ); \
        return NULL;                   \
    }

#define ONYX_GL_INT_3(name) \
    ONYX_DEF(name, (INT, INT, INT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_4(name) \
    ONYX_DEF(name, (INT, INT, INT, INT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_5(name) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_6(name) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, INT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_7(name) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, INT, INT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32, params->data[6].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_8(name) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, INT, INT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32, params->data[6].of.i32, params->data[7].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_9(name) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, INT, INT, INT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32, params->data[6].of.i32, params->data[7].of.i32, params->data[8].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_10(name) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, INT, INT, INT, INT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32, params->data[6].of.i32, params->data[7].of.i32, params->data[8].of.i32, params->data[9].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_FLOAT_1(name)                   \
    ONYX_DEF(name, (FLOAT), ()) {  \
        glad_##name (params->data[0].of.f32); \
        return NULL; \
    }

#define ONYX_GL_FLOAT_INT(name)                   \
    ONYX_DEF(name, (FLOAT, INT), ()) {  \
        glad_##name (params->data[0].of.f32, params->data[1].of.i32); \
        return NULL; \
    }

#define ONYX_GL_FLOAT_2(name)                   \
    ONYX_DEF(name, (FLOAT, FLOAT), ()) {  \
        glad_##name (params->data[0].of.f32, params->data[1].of.f32); \
        return NULL; \
    }

#define ONYX_GL_FLOAT_4(name)                   \
    ONYX_DEF(name, (FLOAT, FLOAT, FLOAT, FLOAT), ()) {  \
        glad_##name (params->data[0].of.f32, params->data[1].of.f32, params->data[2].of.f32, params->data[3].of.f32); \
        return NULL; \
    }

#define ONYX_GL_INT_PTR(name, ptr_type)         \
    ONYX_DEF(name, (INT, PTR), ()) {            \
        glad_##name (params->data[0].of.i32, (ptr_type *) ONYX_PTR(params->data[1].of.i32)); \
        return NULL; \
    }

#define ONYX_GL_INT_PTR_RET_PTR(name, ptr_type) \
    ONYX_DEF(name, (INT, PTR), (INT)) {            \
        results->data[0] = WASM_I32_VAL(glad_##name (params->data[0].of.i32, (ptr_type *) ONYX_PTR(params->data[1].of.i32))); \
        return NULL; \
    }

#define ONYX_GL_INT_PTR_RET_INT(name, ptr_type) \
    ONYX_DEF(name, (INT, PTR), (INT)) {            \
        results->data[0] = WASM_I32_VAL(glad_##name (params->data[0].of.i32, (ptr_type *) ONYX_PTR(params->data[1].of.i32))); \
        return NULL; \
    }

#define ONYX_GL_INT_2_PTR(name, ptr_type)       \
    ONYX_DEF(name, (INT, INT, PTR), ()) {       \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, (ptr_type *) ONYX_PTR(params->data[2].of.i32)); \
        return NULL; \
    }

#define ONYX_GL_INT_3_PTR(name, ptr_type) \
    ONYX_DEF(name, (INT, INT, INT, PTR), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, (ptr_type *) ONYX_PTR(params->data[3].of.i32)); \
        return NULL;                   \
    }

#define ONYX_GL_INT_3_FAUX_PTR(name, ptr_type) \
    ONYX_DEF(name, (INT, INT, INT, PTR), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, (ptr_type *) (unsigned long long) params->data[3].of.i32); \
        return NULL;                   \
    }

#define ONYX_GL_INT_4_PTR(name, ptr_type) \
    ONYX_DEF(name, (INT, INT, INT, INT, PTR), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, (ptr_type *) ONYX_PTR(params->data[4].of.i32)); \
        return NULL;                   \
    }

#define ONYX_GL_INT_5_PTR(name, ptr_type) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, PTR), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, (ptr_type *) ONYX_PTR(params->data[5].of.i32)); \
        return NULL;                   \
    }

#define ONYX_GL_INT_6_PTR(name, ptr_type) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, INT, PTR), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32, (ptr_type *) ONYX_PTR(params->data[6].of.i32)); \
        return NULL;                   \
    }

#define ONYX_GL_INT_7_PTR(name, ptr_type) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, INT, INT, PTR), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32, params->data[6].of.i32, (ptr_type *) ONYX_PTR(params->data[7].of.i32)); \
        return NULL;                   \
    }

#define ONYX_GL_INT_8_PTR(name, ptr_type) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, INT, INT, INT, PTR), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32, params->data[6].of.i32, params->data[7].of.i32, (ptr_type *) ONYX_PTR(params->data[8].of.i32)); \
        return NULL;                   \
    }

#define ONYX_GL_INT_9_PTR(name, ptr_type) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, INT, INT, INT, INT, PTR), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32, params->data[6].of.i32, params->data[7].of.i32, params->data[8].of.i32, (ptr_type *) ONYX_PTR(params->data[9].of.i32)); \
        return NULL;                   \
    }

#define ONYX_GL_INT_10_PTR(name, ptr_type) \
    ONYX_DEF(name, (INT, INT, INT, INT, INT, INT, INT, INT, INT, INT, PTR), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32, params->data[6].of.i32, params->data[7].of.i32, params->data[8].of.i32, params->data[9].of.i32, (ptr_type *) ONYX_PTR(params->data[10].of.i32)); \
        return NULL;                   \
    }

#define ONYX_GL_INT_FLOAT(name) \
    ONYX_DEF(name, (INT, FLOAT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.f32); \
        return NULL; \
    }

#define ONYX_GL_INT_FLOAT_2(name) \
    ONYX_DEF(name, (INT, FLOAT, FLOAT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.f32, params->data[2].of.f32); \
        return NULL; \
    }

#define ONYX_GL_INT_FLOAT_3(name) \
    ONYX_DEF(name, (INT, FLOAT, FLOAT, FLOAT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.f32, params->data[2].of.f32, params->data[3].of.f32); \
        return NULL; \
    }

#define ONYX_GL_INT_FLOAT_4(name) \
    ONYX_DEF(name, (INT, FLOAT, FLOAT, FLOAT, FLOAT), ()) { \
        glad_##name (params->data[0].of.i32, params->data[1].of.f32, params->data[2].of.f32, params->data[3].of.f32, params->data[4].of.f32); \
        return NULL; \
    }

#define ONYX_GL_INT_2_FLOAT(name)       \
    ONYX_DEF(name, (INT, INT, FLOAT), ()) {       \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32); \
        return NULL; \
    }

#define ONYX_GL_INT_2_PTR_INT(name, ptr_type)       \
    ONYX_DEF(name, (INT, INT, PTR, INT), ()) {       \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, (ptr_type *) ONYX_PTR(params->data[2].of.i32), params->data[3].of.i32); \
        return NULL; \
    }

#define ONYX_GL_INT_2_PTR_2(name, p1, p2)       \
    ONYX_DEF(name, (INT, INT, PTR, PTR), ()) {       \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, \
                (p1 *) ONYX_PTR(params->data[2].of.i32), \
                (p2 *) ONYX_PTR(params->data[3].of.i32)); \
        return NULL; \
    }

#define ONYX_GL_INT_2_PTR_3(name, p1, p2, p3)       \
    ONYX_DEF(name, (INT, INT, PTR, PTR, PTR), ()) {       \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, \
                (p1 *) ONYX_PTR(params->data[2].of.i32), \
                (p2 *) ONYX_PTR(params->data[3].of.i32), \
                (p3 *) ONYX_PTR(params->data[4].of.i32)); \
        return NULL; \
    }

#define ONYX_GL_INT_3_PTR_2(name, p1, p2)       \
    ONYX_DEF(name, (INT, INT, INT, PTR, PTR), ()) {       \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, \
                (p1 *) ONYX_PTR(params->data[3].of.i32), \
                (p2 *) ONYX_PTR(params->data[4].of.i32)); \
        return NULL; \
    }

#define ONYX_GL_INT_3_PTR_4(name, p1, p2, p3, p4)       \
    ONYX_DEF(name, (INT, INT, INT, PTR, PTR, PTR, PTR), ()) {       \
        glad_##name (params->data[0].of.i32, params->data[1].of.i32, params->data[2].of.i32, \
                (p1 *) ONYX_PTR(params->data[3].of.i32), \
                (p2 *) ONYX_PTR(params->data[4].of.i32), \
                (p3 *) ONYX_PTR(params->data[5].of.i32), \
                (p4 *) ONYX_PTR(params->data[6].of.i32)); \
        return NULL; \
    }

#define ONYX_GL_INT_PTR_INT_PTR_INT(name, p1, p2) \
    ONYX_DEF(name, (INT, PTR, INT, PTR, INT), ()) { \
        glad_##name (params->data[0].of.i32,                        \
                (p1 *) ONYX_PTR(params->data[1].of.i32),     \
                params->data[2].of.i32,                      \
                (p2 *) ONYX_PTR(params->data[3].of.i32),     \
                params->data[4].of.i32);                     \
        return NULL;                                         \
    }

ONYX_DEF(glInit, (LONG), ()) {
    GLADloadfunc load_sym = (GLADloadfunc) params->data[0].of.i64;
    gladLoadGLES2(load_sym);
    return NULL;
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
ONYX_GL_INT_PTR(glGetAttribLocation, char)
ONYX_GL_INT_PTR(glGetBooleanv, GLboolean)
ONYX_GL_INT_PTR(glGetFloatv, GLfloat)
ONYX_GL_INT_PTR(glGetIntegerv, GLint)
ONYX_GL_INT_PTR_RET_INT(glGetUniformLocation, char)
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
ONYX_GL_INT_PTR(glGetFragDataLocation, char)
ONYX_GL_INT_PTR(glVertexAttribI4iv, GLint)
ONYX_GL_INT_PTR(glVertexAttribI4uiv, GLuint)
ONYX_GL_INT_PTR_RET_PTR(glGetUniformBlockIndex, char)
ONYX_GL_INT_3(glDrawArrays)
ONYX_GL_INT_3(glStencilFunc)
ONYX_GL_INT_3(glStencilOp)
ONYX_GL_INT_3(glFlushMappedBufferRange)
ONYX_GL_INT_3(glBindBufferBase)
ONYX_GL_INT_3(glSamplerParameteri)
ONYX_GL_INT_3(glProgramParameteri)
ONYX_GL_INT_3(glTexParameteri)
ONYX_GL_INT_4(glBlendFuncSeparate)
ONYX_GL_INT_4(glColorMask)
ONYX_GL_INT_4(glScissor)
ONYX_GL_INT_4(glStencilFuncSeparate)
ONYX_GL_INT_4(glViewport)
ONYX_GL_INT_4(glMapBufferRange)
ONYX_GL_INT_4(glFramebufferRenderbuffer)
ONYX_GL_INT_4(glRenderbufferStorage)
ONYX_GL_INT_4(glStencilOpSeparate)

ONYX_GL_INT_2_PTR(glBindAttribLocation, char)
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
ONYX_GL_INT_2_PTR(glGetIntegeri_v, GLint)
ONYX_GL_INT_2_PTR(glGetVertexAttribIiv, GLint)
ONYX_GL_INT_2_PTR(glGetVertexAttribIuiv, GLuint)
ONYX_GL_INT_2_PTR(glGetUniformuiv, GLuint)
ONYX_GL_INT_2_PTR(glUniform1uiv, GLuint)
ONYX_GL_INT_2_PTR(glUniform2uiv, GLuint)
ONYX_GL_INT_2_PTR(glUniform3uiv, GLuint)
ONYX_GL_INT_2_PTR(glUniform4uiv, GLuint)
ONYX_GL_INT_2_PTR(glClearBufferiv, GLint)
ONYX_GL_INT_2_PTR(glClearBufferuiv, GLuint)
ONYX_GL_INT_2_PTR(glClearBufferfv, GLfloat)
ONYX_GL_INT_2_PTR(glInvalidateFramebuffer, GLenum)
ONYX_GL_INT_3_PTR(glBufferSubData, void)
ONYX_GL_INT_3_FAUX_PTR(glDrawElements, void)
ONYX_GL_INT_3_PTR(glGetFramebufferAttachmentParameteriv, GLint)
ONYX_GL_INT_2_FLOAT(glSamplerParameterf)
ONYX_GL_INT_2_FLOAT(glTexParameterf)

ONYX_GL_INT_2_PTR_INT(glBufferData, void)
ONYX_GL_INT_7_PTR(glCompressedTexImage2D, void)
ONYX_GL_INT_8_PTR(glCompressedTexSubImage2D, void)
ONYX_GL_INT_8(glCopyTexImage2D)
ONYX_GL_INT_8(glCopyTexSubImage2D)
ONYX_GL_INT_5(glFramebufferTexture2D)
ONYX_GL_INT_3_PTR_4(glGetActiveAttrib, GLsizei, GLint, GLenum, char)
ONYX_GL_INT_3_PTR_4(glGetActiveUniform, GLsizei, GLint, GLenum, char)
ONYX_GL_INT_2_PTR_2(glGetAttachedShaders, GLsizei, GLuint)
ONYX_GL_INT_2_PTR_2(glGetProgramInfoLog, GLsizei, char)
ONYX_GL_INT_2_PTR_2(glGetShaderInfoLog, GLsizei, char) 
ONYX_GL_INT_2_PTR_2(glGetShaderPrecisionFormat, GLint, GLint)
ONYX_GL_INT_2_PTR_2(glGetShaderSource, GLsizei, char)
ONYX_GL_INT_6_PTR(glReadPixels, void)
ONYX_GL_FLOAT_INT(glSampleCoverage)
ONYX_GL_INT_PTR_INT_PTR_INT(glShaderBinary, GLuint, void)
ONYX_GL_INT_8_PTR(glTexImage2D, void)
ONYX_GL_INT_8_PTR(glTexSubImage2D, void)
ONYX_GL_INT_6(glVertexAttribPointer)
ONYX_GL_INT_FLOAT(glUniform1f)
ONYX_GL_INT_2(glUniform1i)
ONYX_GL_INT_FLOAT_2(glUniform2f)
ONYX_GL_INT_3(glUniform2i)
ONYX_GL_INT_FLOAT_3(glUniform3f)
ONYX_GL_INT_4(glUniform3i)
ONYX_GL_INT_FLOAT_4(glUniform4f)
ONYX_GL_INT_5(glUniform4i)
ONYX_GL_INT_2(glUniform1ui)
ONYX_GL_INT_3(glUniform2ui)
ONYX_GL_INT_4(glUniform3ui)
ONYX_GL_INT_5(glUniform4ui)
ONYX_GL_INT_3_PTR(glUniformMatrix2fv, GLfloat)
ONYX_GL_INT_3_PTR(glUniformMatrix3fv, GLfloat)
ONYX_GL_INT_3_PTR(glUniformMatrix4fv, GLfloat)
ONYX_GL_INT_FLOAT(glVertexAttrib1f)
ONYX_GL_INT_FLOAT_2(glVertexAttrib2f)
ONYX_GL_INT_FLOAT_3(glVertexAttrib3f)
ONYX_GL_INT_FLOAT_4(glVertexAttrib4f)
ONYX_GL_INT_3_PTR(glUniformMatrix2x3fv, GLfloat)
ONYX_GL_INT_3_PTR(glUniformMatrix3x2fv, GLfloat)
ONYX_GL_INT_3_PTR(glUniformMatrix2x4fv, GLfloat)
ONYX_GL_INT_3_PTR(glUniformMatrix4x2fv, GLfloat)
ONYX_GL_INT_3_PTR(glUniformMatrix3x4fv, GLfloat)
ONYX_GL_INT_3_PTR(glUniformMatrix4x3fv, GLfloat)


// Open3: GLES
ONYX_GL_INT_6(glDrawRangeElements)
ONYX_GL_INT_9_PTR(glTexImage3D, void)
ONYX_GL_INT_10_PTR(glTexSubImage3D, void)
ONYX_GL_INT_9(glCopyTexSubImage3D)
ONYX_GL_INT_8_PTR(glCompressedTexImage3D, void)
ONYX_GL_INT_10_PTR(glCompressedTexSubImage3D, void)
ONYX_GL_INT_10(glBlitFramebuffer)
ONYX_GL_INT_5(glRenderbufferStorageMultisample)
ONYX_GL_INT_5(glFramebufferTextureLayer)
ONYX_GL_INT_5(glBindBufferRange)
ONYX_GL_INT_3_PTR_4(glGetTransformFeedbackVarying, GLsizei, GLsizei, GLenum, char)
ONYX_GL_INT_5(glVertexAttribIPointer)
ONYX_GL_INT_5(glVertexAttribI4i)
ONYX_GL_INT_5(glVertexAttribI4ui)
// ONYX_GL_INT_2_FLOAT_INT(glClearBufferfi)
ONYX_GL_INT_5(glCopyBufferSubData)
// ONYX_GL_INT_2_PTR_INT_PTR(glGetActiveUniformsiv, GLuint, GLint)
ONYX_GL_INT_3_PTR(glGetActiveUniformBlockiv, GLint)
ONYX_GL_INT_3_PTR_2(glGetActiveUniformBlockName, GLsizei, char)
ONYX_GL_INT_3(glUniformBlockBinding)
ONYX_GL_INT_4(glDrawArraysInstanced)
ONYX_GL_INT_5(glDrawElementsInstanced)
ONYX_GL_INT_2_PTR_3(glGetProgramBinary, GLsizei, GLenum, void)
ONYX_GL_INT_2_PTR_INT(glProgramBinary, void)
// ONYX_GL_INT_2_PTR_INT_4(glInvalidateSubFramebuffer, GLenum)
ONYX_GL_INT_5(glTexStorage2D)
ONYX_GL_INT_6(glTexStorage3D)
ONYX_GL_INT_4_PTR(glGetInternalformativ, GLint)

ONYX_DEF(glShaderSource, (INT, INT, PTR, PTR), ()) {
    GLsizei count = params->data[1].of.i32;
    int base_ptr = *(int *) ONYX_PTR(params->data[2].of.i32);
    char** strs = alloca(count * sizeof(char *));
    for (int i=0; i<count; i++) {
        strs[i] = (char *) ONYX_PTR(base_ptr + i * 4);
    }

    glad_glShaderSource(params->data[0].of.i32, count, strs, (GLint *) ONYX_PTR(params->data[3].of.i32));
    return NULL;
}

ONYX_DEF(glGetUniformIndices, (INT, INT, PTR, PTR), ()) {
    GLsizei count = params->data[1].of.i32;
    int base_ptr = *(int *) ONYX_PTR(params->data[2].of.i32);
    char** strs = alloca(count * sizeof(char *));
    for (int i=0; i<count; i++) {
        strs[i] = (char *) ONYX_PTR(base_ptr + i * 4);
    }
    
    glad_glGetUniformIndices(params->data[0].of.i32, count, (const char *const*) strs, (int *) ONYX_PTR(params->data[3].of.i32));
    return NULL;
}

// glGetBufferPointerv :: (target: GLenum, pname: GLenum, params: ^rawptr) -> void ---
// glGetVertexAttribPointerv :: (index: GLuint, pname: GLenum, pointer: ^rawptr) -> void ---
// glTransformFeedbackVaryings       :: (program: GLuint, count: GLsizei, varyings: ^^GLchar, bufferMode: GLenum) -> void ---

// Hmm...
// glClientWaitSync                  :: (sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> GLenum ---
// glWaitSync                        :: (sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> void ---
// glGetSynciv                       :: (sync: GLsync, pname: GLenum, bufSize: GLsizei, length: ^GLsizei, values: ^GLint) -> void ---
// ONYX_GL_INT_RET_LONG(glGetString)
// ONYX_GL_INT_2_RET_LONG(glGetStringi)


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
    // ONYX_FUNC(glGetString)
    ONYX_FUNC(glGetTexParameterfv)
    ONYX_FUNC(glGetTexParameteriv)
    ONYX_FUNC(glGetUniformfv)
    ONYX_FUNC(glGetUniformiv)
    ONYX_FUNC(glGetUniformLocation)
    ONYX_FUNC(glGetVertexAttribfv)
    ONYX_FUNC(glGetVertexAttribiv)
    // ONYX_FUNC(glGetVertexAttribPointerv)
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


    // Open3: GLES
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
    // ONYX_FUNC(glGetBufferPointerv)
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
    // ONYX_FUNC(glTransformFeedbackVaryings)
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
    // ONYX_FUNC(glClearBufferfi)
    // ONYX_FUNC(glGetStringi)
    ONYX_FUNC(glCopyBufferSubData)
    ONYX_FUNC(glGetUniformIndices)
    // ONYX_FUNC(glGetActiveUniformsiv)
    ONYX_FUNC(glGetUniformBlockIndex)
    ONYX_FUNC(glGetActiveUniformBlockiv)
    ONYX_FUNC(glGetActiveUniformBlockName)
    ONYX_FUNC(glUniformBlockBinding)
    ONYX_FUNC(glDrawArraysInstanced)
    ONYX_FUNC(glDrawElementsInstanced)
    //ONYX_FUNC(glFenceSync)
    //ONYX_FUNC(glIsSync)
    //ONYX_FUNC(glDeleteSync)
    //ONYX_FUNC(glClientWaitSync)
    //ONYX_FUNC(glWaitSync)
    ONYX_FUNC(glGetInteger64v)
    // ONYX_FUNC(glGetSynciv)
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
    // ONYX_FUNC(glInvalidateSubFramebuffer)
    ONYX_FUNC(glTexStorage2D)
    ONYX_FUNC(glTexStorage3D)
    ONYX_FUNC(glGetInternalformativ)

    NULL
};
