#define ONYX_LIBRARY_NAME onyx_openal
#include "onyx_library.h"
#include <AL/al.h>
#include <AL/alc.h>

#define P(i, k) (params->data[i].of.k)

ONYX_DEF(alGenBuffers, (INT, PTR), ()) {
    alGenBuffers(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(alDeleteBuffers, (INT, PTR), ()) {
    alDeleteBuffers(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(alIsBuffer, (INT), (BOOL)) {
    results->data[0] = WASM_I32_VAL(alIsBuffer(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(alBufferData, (INT, INT, PTR, INT, INT), ()) {
    alBufferData(
        params->data[0].of.i32,
        params->data[1].of.i32,
        ONYX_PTR(params->data[2].of.i32),
        params->data[3].of.i32,
        params->data[4].of.i32);
    return NULL;
}

ONYX_DEF(alBufferf,  (INT, INT, FLOAT), ()) { alBufferf(P(0, i32), P(1, i32), P(2, f32)); return NULL; }
ONYX_DEF(alBuffer3f, (INT, INT, FLOAT, FLOAT, FLOAT), ()) { alBuffer3f(P(0, i32), P(1, i32), P(2, f32), P(3, f32), P(4, f32)); return NULL; }
ONYX_DEF(alBufferfv, (INT, INT, PTR), ()) { alBufferfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alBufferi,  (INT, INT, INT), ()) { alBufferi(P(0, i32), P(1, i32), P(2, i32)); return NULL; }
ONYX_DEF(alBuffer3i, (INT, INT, INT, INT, INT), ()) { alBuffer3i(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32)); return NULL; }
ONYX_DEF(alBufferiv, (INT, INT, PTR), ()) { alBufferiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alGetBufferf,  (INT, INT, PTR), ()) { alGetBufferf(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alGetBuffer3f, (INT, INT, PTR, PTR, PTR), ()) { alGetBuffer3f(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32))); return NULL; }
ONYX_DEF(alGetBufferfv, (INT, INT, PTR), ()) { alGetBufferfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alGetBufferi,  (INT, INT, PTR), ()) { alGetBufferi(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alGetBuffer3i, (INT, INT, PTR, PTR, PTR), ()) { alGetBuffer3i(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32))); return NULL; }
ONYX_DEF(alGetBufferiv, (INT, INT, PTR), ()) { alGetBufferiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }

ONYX_DEF(alGenSources, (INT, PTR), ()) {
    alGenSources(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(alDeleteSources, (INT, PTR), ()) {
    alDeleteSources(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(alIsSource, (INT), (BOOL)) {
    results->data[0] = WASM_I32_VAL(alIsSource(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(alSourcef,  (INT, INT, FLOAT), ()) { alSourcef(P(0, i32), P(1, i32), P(2, f32)); return NULL; }
ONYX_DEF(alSource3f, (INT, INT, FLOAT, FLOAT, FLOAT), ()) { alSource3f(P(0, i32), P(1, i32), P(2, f32), P(3, f32), P(4, f32)); return NULL; }
ONYX_DEF(alSourcefv, (INT, INT, PTR), ()) { alSourcefv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alSourcei,  (INT, INT, INT), ()) { alSourcei(P(0, i32), P(1, i32), P(2, i32)); return NULL; }
ONYX_DEF(alSource3i, (INT, INT, INT, INT, INT), ()) { alSource3i(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32)); return NULL; }
ONYX_DEF(alSourceiv, (INT, INT, PTR), ()) { alSourceiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alGetSourcef,  (INT, INT, PTR), ()) { alGetSourcef(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alGetSource3f, (INT, INT, PTR, PTR, PTR), ()) { alGetSource3f(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32))); return NULL; }
ONYX_DEF(alGetSourcefv, (INT, INT, PTR), ()) { alGetSourcefv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alGetSourcei,  (INT, INT, PTR), ()) { alGetSourcei(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alGetSource3i, (INT, INT, PTR, PTR, PTR), ()) { alGetSource3i(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32))); return NULL; }
ONYX_DEF(alGetSourceiv, (INT, INT, PTR), ()) { alGetSourceiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }

ONYX_DEF(alSourcePlay,  (INT),      ()) { alSourcePlay(P(0, i32)); return NULL; }
ONYX_DEF(alSourcePlayv, (INT, PTR), ()) { alSourcePlayv(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alSourcePause,  (INT),      ()) { alSourcePause(P(0, i32)); return NULL; }
ONYX_DEF(alSourcePausev, (INT, PTR), ()) { alSourcePausev(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alSourceStop,  (INT),      ()) { alSourceStop(P(0, i32)); return NULL; }
ONYX_DEF(alSourceStopv, (INT, PTR), ()) { alSourceStopv(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alSourceRewind,  (INT),      ()) { alSourceRewind(P(0, i32)); return NULL; }
ONYX_DEF(alSourceRewindv, (INT, PTR), ()) { alSourceRewindv(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alSourceQueueBuffers,   (INT, INT, PTR), ()) { alSourceQueueBuffers(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }
ONYX_DEF(alSourceUnqueueBuffers, (INT, INT, PTR), ()) { alSourceUnqueueBuffers(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))); return NULL; }

ONYX_DEF(alListenerf,  (INT, FLOAT), ()) { alListenerf(P(0, i32), P(1, f32)); return NULL; }
ONYX_DEF(alListener3f, (INT, FLOAT, FLOAT, FLOAT), ()) { alListener3f(P(0, i32), P(1, f32), P(2, f32), P(3, f32)); return NULL; }
ONYX_DEF(alListenerfv, (INT, PTR), ()) { alListenerfv(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alListeneri,  (INT, INT), ()) { alListeneri(P(0, i32), P(1, i32)); return NULL; }
ONYX_DEF(alListener3i, (INT, INT, INT, INT), ()) { alListener3i(P(0, i32), P(1, i32), P(2, i32), P(3, i32)); return NULL; }
ONYX_DEF(alListeneriv, (INT, PTR), ()) { alListeneriv(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }

ONYX_DEF(alGetListenerf,  (INT, PTR), ()) { alGetListenerf(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alGetListener3f, (INT, PTR, PTR, PTR), ()) { alGetListener3f(P(0, i32), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32))); return NULL; }
ONYX_DEF(alGetListenerfv, (INT, PTR), ()) { alGetListenerfv(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alGetListeneri,  (INT, PTR), ()) { alGetListeneri(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alGetListener3i, (INT, PTR, PTR, PTR), ()) { alGetListener3i(P(0, i32), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32))); return NULL; }
ONYX_DEF(alGetListeneriv, (INT, PTR), ()) { alGetListeneriv(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }

ONYX_DEF(alEnable,  (INT), ()) { alEnable(P(0, i32)); return NULL; }
ONYX_DEF(alDisable, (INT), ()) { alDisable(P(0, i32)); return NULL; }
ONYX_DEF(alIsEnabled, (INT), (BOOL)) { results->data[0] = WASM_I32_VAL(alIsEnabled(P(0, i32))); return NULL; }
ONYX_DEF(alGetBoolean, (INT), (BOOL)) { results->data[0] = WASM_I32_VAL(alGetBoolean(P(0, i32))); return NULL; }
ONYX_DEF(alGetDouble, (INT), (DOUBLE)) { results->data[0] = WASM_F64_VAL(alGetDouble(P(0, i32))); return NULL; }
ONYX_DEF(alGetFloat, (INT), (FLOAT)) { results->data[0] = WASM_F32_VAL(alGetFloat(P(0, i32))); return NULL; }
ONYX_DEF(alGetInteger, (INT), (INT)) { results->data[0] = WASM_I32_VAL(alGetInteger(P(0, i32))); return NULL; }
ONYX_DEF(alGetBooleanv, (INT, PTR), ()) { alGetBooleanv(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alGetDoublev, (INT, PTR), ()) { alGetDoublev(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alGetFloatv, (INT, PTR), ()) { alGetFloatv(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alGetIntegerv, (INT, PTR), ()) { alGetIntegerv(P(0, i32), ONYX_PTR(P(1, i32))); return NULL; }
ONYX_DEF(alDistanceModel, (INT), ()) { alDistanceModel(P(0, i32)); return NULL; }
ONYX_DEF(alDopplerFactor, (FLOAT), ()) { alDistanceModel(P(0, f32)); return NULL; }
ONYX_DEF(alSpeedOfSound, (FLOAT), ()) { alDistanceModel(P(0, f32)); return NULL; }

ONYX_DEF(alGetError, (), (INT)) { results->data[0] = WASM_I32_VAL(alGetError()); return NULL; }
ONYX_DEF(alGetString, (INT), (LONG)) {
    wasm_val_init_ptr(&results->data[0], (void *) alGetString(P(0, i32)));
    return NULL;
}

ONYX_DEF(alcCreateContext, (LONG, PTR), (LONG)) { wasm_val_init_ptr(&results->data[0], alcCreateContext((ALCdevice *) P(0, i64), ONYX_PTR(P(1, i32)))); return NULL; }
ONYX_DEF(alcMakeContextCurrent, (LONG), (BOOL)) { results->data[0] = WASM_I32_VAL(alcMakeContextCurrent((ALCcontext *) P(0, i64))); return NULL; }
ONYX_DEF(alcProcessContext, (LONG), ()) { alcProcessContext((ALCcontext *) P(0, i64)); return NULL; }
ONYX_DEF(alcSuspendContext, (LONG), ()) { alcSuspendContext((ALCcontext *) P(0, i64)); return NULL; }
ONYX_DEF(alcDestroyContext, (LONG), ()) { alcDestroyContext((ALCcontext *) P(0, i64)); return NULL; }
ONYX_DEF(alcGetCurrentContext, (), (LONG)) { wasm_val_init_ptr(&results->data[0], alcGetCurrentContext()); return NULL; }
ONYX_DEF(alcGetContextsDevice, (LONG), (LONG)) { wasm_val_init_ptr(&results->data[0], alcGetContextsDevice((ALCcontext *) P(0, i64))); return NULL; }
ONYX_DEF(alcGetError, (LONG), (INT)) { results->data[0] = WASM_I32_VAL(alcGetError((ALCdevice *) P(0, i64))); return NULL; }
ONYX_DEF(alcOpenDevice, (PTR), (LONG)) { wasm_val_init_ptr(&results->data[0], alcOpenDevice(ONYX_PTR(P(0, i32)))); return NULL; }
ONYX_DEF(alcCloseDevice, (LONG), (BOOL)) { results->data[0] = WASM_I32_VAL(alcCloseDevice((ALCdevice *) P(0, i64))); return NULL; }

ONYX_DEF(alcIsExtensionPresent, (LONG, PTR), (BOOL)) { results->data[0] = WASM_I32_VAL(alcIsExtensionPresent((ALCdevice *) P(0, i64), ONYX_PTR(P(1, i32)))); return NULL; }
ONYX_DEF(alcGetProcAddress, (LONG, PTR), (LONG)) { wasm_val_init_ptr(&results->data[0], alcGetProcAddress((ALCdevice *) P(0, i64), ONYX_PTR(P(1, i32)))); return NULL; }
ONYX_DEF(alcGetEnumValue, (LONG, PTR), (INT)) { results->data[0] = WASM_I32_VAL(alcGetEnumValue((ALCdevice *) P(0, i64), ONYX_PTR(P(1, i32)))); return NULL; }
ONYX_DEF(alcGetIntegerv, (LONG, INT, INT, PTR), ()) { alcGetIntegerv((ALCdevice *) P(0, i64), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32))); return NULL; }

ONYX_DEF(alcCaptureOpenDevice, (PTR, INT, INT, INT), (LONG)) {
    wasm_val_init_ptr(&results->data[0], alcCaptureOpenDevice(ONYX_PTR(P(0, i32)), P(1, i32), P(2, i32), P(3, i32)));
    return NULL;
}
ONYX_DEF(alcCaptureCloseDevice, (LONG), (BOOL)) { results->data[0] = WASM_I32_VAL(alcCaptureCloseDevice((ALCdevice *) P(0, i64))); return NULL; }
ONYX_DEF(alcCaptureStart, (LONG), ()) { alcCaptureStart((ALCdevice *) P(0, i64)); return NULL; }
ONYX_DEF(alcCaptureStop, (LONG), ()) { alcCaptureStop((ALCdevice *) P(0, i64)); return NULL; }
ONYX_DEF(alcCaptureSamples, (LONG, PTR, INT), ()) { alcCaptureSamples((ALCdevice *) P(0, i64), ONYX_PTR(P(1, i32)), P(2, i32)); return NULL; }
ONYX_DEF(alcGetString, (LONG, INT), (LONG)) {
    wasm_val_init_ptr(&results->data[0], (void *) alcGetString((ALCdevice *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_LIBRARY {
    ONYX_FUNC(alGenBuffers)
    ONYX_FUNC(alDeleteBuffers)
    ONYX_FUNC(alIsBuffer)
    ONYX_FUNC(alBufferData)
    ONYX_FUNC(alBufferf)
    ONYX_FUNC(alBuffer3f)
    ONYX_FUNC(alBufferfv)
    ONYX_FUNC(alBufferi)
    ONYX_FUNC(alBuffer3i)
    ONYX_FUNC(alBufferiv)
    ONYX_FUNC(alGetBufferf)
    ONYX_FUNC(alGetBuffer3f)
    ONYX_FUNC(alGetBufferfv)
    ONYX_FUNC(alGetBufferi)
    ONYX_FUNC(alGetBuffer3i)
    ONYX_FUNC(alGetBufferiv)

    ONYX_FUNC(alGenSources)
    ONYX_FUNC(alDeleteSources)
    ONYX_FUNC(alIsSource)
    ONYX_FUNC(alSourcef)
    ONYX_FUNC(alSource3f)
    ONYX_FUNC(alSourcefv)
    ONYX_FUNC(alSourcei)
    ONYX_FUNC(alSource3i)
    ONYX_FUNC(alSourceiv)
    ONYX_FUNC(alGetSourcef)
    ONYX_FUNC(alGetSource3f)
    ONYX_FUNC(alGetSourcefv)
    ONYX_FUNC(alGetSourcei)
    ONYX_FUNC(alGetSource3i)
    ONYX_FUNC(alGetSourceiv)
    ONYX_FUNC(alSourcePlay)
    ONYX_FUNC(alSourcePlayv)
    ONYX_FUNC(alSourcePause)
    ONYX_FUNC(alSourcePausev)
    ONYX_FUNC(alSourceStop)
    ONYX_FUNC(alSourceStopv)
    ONYX_FUNC(alSourceRewind)
    ONYX_FUNC(alSourceRewindv)
    ONYX_FUNC(alSourceQueueBuffers)
    ONYX_FUNC(alSourceUnqueueBuffers)

    ONYX_FUNC(alListenerf)
    ONYX_FUNC(alListener3f)
    ONYX_FUNC(alListenerfv)
    ONYX_FUNC(alListeneri)
    ONYX_FUNC(alListener3i)
    ONYX_FUNC(alListeneriv)
    ONYX_FUNC(alGetListenerf)
    ONYX_FUNC(alGetListener3f)
    ONYX_FUNC(alGetListenerfv)
    ONYX_FUNC(alGetListeneri)
    ONYX_FUNC(alGetListener3i)
    ONYX_FUNC(alGetListeneriv)

    ONYX_FUNC(alEnable)
    ONYX_FUNC(alDisable)
    ONYX_FUNC(alIsEnabled)
    ONYX_FUNC(alGetBoolean)
    ONYX_FUNC(alGetDouble)
    ONYX_FUNC(alGetFloat)
    ONYX_FUNC(alGetInteger)
    ONYX_FUNC(alGetBooleanv)
    ONYX_FUNC(alGetDoublev)
    ONYX_FUNC(alGetFloatv)
    ONYX_FUNC(alGetIntegerv)
    ONYX_FUNC(alDistanceModel)
    ONYX_FUNC(alDopplerFactor)
    ONYX_FUNC(alSpeedOfSound)

    ONYX_FUNC(alGetError)

    ONYX_FUNC(alcCreateContext)
    ONYX_FUNC(alcMakeContextCurrent)
    ONYX_FUNC(alcProcessContext)
    ONYX_FUNC(alcSuspendContext)
    ONYX_FUNC(alcDestroyContext)
    ONYX_FUNC(alcGetCurrentContext)
    ONYX_FUNC(alcGetContextsDevice)
    ONYX_FUNC(alcGetError)

    ONYX_FUNC(alcOpenDevice)
    ONYX_FUNC(alcCloseDevice)

    ONYX_FUNC(alcIsExtensionPresent)
    ONYX_FUNC(alcGetProcAddress)
    ONYX_FUNC(alcGetEnumValue)
    ONYX_FUNC(alcGetIntegerv)

    ONYX_FUNC(alcCaptureOpenDevice)
    ONYX_FUNC(alcCaptureCloseDevice)
    ONYX_FUNC(alcCaptureStart)
    ONYX_FUNC(alcCaptureStop)
    ONYX_FUNC(alcCaptureSamples)

    NULL
};