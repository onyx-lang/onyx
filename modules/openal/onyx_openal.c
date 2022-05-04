//
// THIS FILE WAS AUTOMATICALLY GENERATED.
//

#include <AL/al.h>
#include <AL/alc.h>


#define ONYX_LIBRARY_NAME onyx_openal
#include "onyx_library.h"

#define P(i, k) (params->data[i].of.k)

ONYX_DEF(alGenBuffers, (WASM_I32, WASM_I32), ()) {
    alGenBuffers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alDeleteBuffers, (WASM_I32, WASM_I32), ()) {
    alDeleteBuffers(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alIsBuffer, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alIsBuffer(P(0, i32)));
    return NULL;
}

ONYX_DEF(alBufferData, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    alBufferData(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(alBufferf, (WASM_I32, WASM_I32, WASM_F32), ()) {
    alBufferf(P(0, i32), P(1, i32), P(2, f32));
    return NULL;
}

ONYX_DEF(alBuffer3f, (WASM_I32, WASM_I32, WASM_F32, WASM_F32, WASM_F32), ()) {
    alBuffer3f(P(0, i32), P(1, i32), P(2, f32), P(3, f32), P(4, f32));
    return NULL;
}

ONYX_DEF(alBufferfv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alBufferfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alBufferi, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alBufferi(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(alBuffer3i, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    alBuffer3i(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(alBufferiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alBufferiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alGetBufferf, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetBufferf(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alGetBuffer3f, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetBuffer3f(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(alGetBufferfv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetBufferfv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alGetBufferi, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetBufferi(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alGetBuffer3i, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetBuffer3i(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(alGetBufferiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetBufferiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alGenSources, (WASM_I32, WASM_I32), ()) {
    alGenSources(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alDeleteSources, (WASM_I32, WASM_I32), ()) {
    alDeleteSources(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alIsSource, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alIsSource(P(0, i32)));
    return NULL;
}

ONYX_DEF(alSourcef, (WASM_I32, WASM_I32, WASM_F32), ()) {
    alSourcef(P(0, i32), P(1, i32), P(2, f32));
    return NULL;
}

ONYX_DEF(alSource3f, (WASM_I32, WASM_I32, WASM_F32, WASM_F32, WASM_F32), ()) {
    alSource3f(P(0, i32), P(1, i32), P(2, f32), P(3, f32), P(4, f32));
    return NULL;
}

ONYX_DEF(alSourcefv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alSourcefv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alSourcei, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alSourcei(P(0, i32), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(alSource3i, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    alSource3i(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(alSourceiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alSourceiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alGetSourcef, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetSourcef(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alGetSource3f, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetSource3f(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(alGetSourcefv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetSourcefv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alGetSourcei, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetSourcei(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alGetSource3i, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetSource3i(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(alGetSourceiv, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetSourceiv(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alSourcePlay, (WASM_I32), ()) {
    alSourcePlay(P(0, i32));
    return NULL;
}

ONYX_DEF(alSourcePlayv, (WASM_I32, WASM_I32), ()) {
    alSourcePlayv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alSourcePause, (WASM_I32), ()) {
    alSourcePause(P(0, i32));
    return NULL;
}

ONYX_DEF(alSourcePausev, (WASM_I32, WASM_I32), ()) {
    alSourcePausev(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alSourceStop, (WASM_I32), ()) {
    alSourceStop(P(0, i32));
    return NULL;
}

ONYX_DEF(alSourceStopv, (WASM_I32, WASM_I32), ()) {
    alSourceStopv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alSourceRewind, (WASM_I32), ()) {
    alSourceRewind(P(0, i32));
    return NULL;
}

ONYX_DEF(alSourceRewindv, (WASM_I32, WASM_I32), ()) {
    alSourceRewindv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alSourceQueueBuffers, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alSourceQueueBuffers(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alSourceUnqueueBuffers, (WASM_I32, WASM_I32, WASM_I32), ()) {
    alSourceUnqueueBuffers(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(alListenerf, (WASM_I32, WASM_F32), ()) {
    alListenerf(P(0, i32), P(1, f32));
    return NULL;
}

ONYX_DEF(alListener3f, (WASM_I32, WASM_F32, WASM_F32, WASM_F32), ()) {
    alListener3f(P(0, i32), P(1, f32), P(2, f32), P(3, f32));
    return NULL;
}

ONYX_DEF(alListenerfv, (WASM_I32, WASM_I32), ()) {
    alListenerfv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alListeneri, (WASM_I32, WASM_I32), ()) {
    alListeneri(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(alListener3i, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    alListener3i(P(0, i32), P(1, i32), P(2, i32), P(3, i32));
    return NULL;
}

ONYX_DEF(alListeneriv, (WASM_I32, WASM_I32), ()) {
    alListeneriv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alGetListenerf, (WASM_I32, WASM_I32), ()) {
    alGetListenerf(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alGetListener3f, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetListener3f(P(0, i32), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(alGetListenerfv, (WASM_I32, WASM_I32), ()) {
    alGetListenerfv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alGetListeneri, (WASM_I32, WASM_I32), ()) {
    alGetListeneri(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alGetListener3i, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    alGetListener3i(P(0, i32), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(alGetListeneriv, (WASM_I32, WASM_I32), ()) {
    alGetListeneriv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alEnable, (WASM_I32), ()) {
    alEnable(P(0, i32));
    return NULL;
}

ONYX_DEF(alDisable, (WASM_I32), ()) {
    alDisable(P(0, i32));
    return NULL;
}

ONYX_DEF(alIsEnabled, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alIsEnabled(P(0, i32)));
    return NULL;
}

ONYX_DEF(alGetBoolean, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alGetBoolean(P(0, i32)));
    return NULL;
}

ONYX_DEF(alGetDouble, (WASM_I32), (WASM_F64)) {
    results->data[0] = WASM_F64_VAL(alGetDouble(P(0, i32)));
    return NULL;
}

ONYX_DEF(alGetFloat, (WASM_I32), (WASM_F32)) {
    results->data[0] = WASM_F32_VAL(alGetFloat(P(0, i32)));
    return NULL;
}

ONYX_DEF(alGetInteger, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alGetInteger(P(0, i32)));
    return NULL;
}

ONYX_DEF(alGetBooleanv, (WASM_I32, WASM_I32), ()) {
    alGetBooleanv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alGetDoublev, (WASM_I32, WASM_I32), ()) {
    alGetDoublev(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alGetFloatv, (WASM_I32, WASM_I32), ()) {
    alGetFloatv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alGetIntegerv, (WASM_I32, WASM_I32), ()) {
    alGetIntegerv(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(alDistanceModel, (WASM_I32), ()) {
    alDistanceModel(P(0, i32));
    return NULL;
}

ONYX_DEF(alDopplerFactor, (WASM_F32), ()) {
    alDopplerFactor(P(0, f32));
    return NULL;
}

ONYX_DEF(alSpeedOfSound, (WASM_F32), ()) {
    alSpeedOfSound(P(0, f32));
    return NULL;
}

ONYX_DEF(alGetError, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alGetError());
    return NULL;
}

ONYX_DEF(alcCreateContext, (WASM_I64, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(alcCreateContext((ALCdevice *) P(0, i64), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(alcMakeContextCurrent, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alcMakeContextCurrent((ALCcontext *) P(0, i64)));
    return NULL;
}

ONYX_DEF(alcProcessContext, (WASM_I64), ()) {
    alcProcessContext((ALCcontext *) P(0, i64));
    return NULL;
}

ONYX_DEF(alcSuspendContext, (WASM_I64), ()) {
    alcSuspendContext((ALCcontext *) P(0, i64));
    return NULL;
}

ONYX_DEF(alcDestroyContext, (WASM_I64), ()) {
    alcDestroyContext((ALCcontext *) P(0, i64));
    return NULL;
}

ONYX_DEF(alcGetCurrentContext, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(alcGetCurrentContext());
    return NULL;
}

ONYX_DEF(alcGetContextsDevice, (WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(alcGetContextsDevice((ALCcontext *) P(0, i64)));
    return NULL;
}

ONYX_DEF(alcGetError, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alcGetError((ALCdevice *) P(0, i64)));
    return NULL;
}

ONYX_DEF(alcOpenDevice, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(alcOpenDevice(ONYX_PTR(P(0, i32))));
    return NULL;
}

ONYX_DEF(alcCloseDevice, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alcCloseDevice((ALCdevice *) P(0, i64)));
    return NULL;
}

ONYX_DEF(alcIsExtensionPresent, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alcIsExtensionPresent((ALCdevice *) P(0, i64), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(alcGetProcAddress, (WASM_I64, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(alcGetProcAddress((ALCdevice *) P(0, i64), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(alcGetEnumValue, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alcGetEnumValue((ALCdevice *) P(0, i64), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(alcGetIntegerv, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), ()) {
    alcGetIntegerv((ALCdevice *) P(0, i64), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(alcCaptureOpenDevice, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(alcCaptureOpenDevice(ONYX_PTR(P(0, i32)), P(1, i32), P(2, i32), P(3, i32)));
    return NULL;
}

ONYX_DEF(alcCaptureCloseDevice, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(alcCaptureCloseDevice((ALCdevice *) P(0, i64)));
    return NULL;
}

ONYX_DEF(alcCaptureStart, (WASM_I64), ()) {
    alcCaptureStart((ALCdevice *) P(0, i64));
    return NULL;
}

ONYX_DEF(alcCaptureStop, (WASM_I64), ()) {
    alcCaptureStop((ALCdevice *) P(0, i64));
    return NULL;
}

ONYX_DEF(alcCaptureSamples, (WASM_I64, WASM_I32, WASM_I32), ()) {
    alcCaptureSamples((ALCdevice *) P(0, i64), ONYX_PTR(P(1, i32)), P(2, i32));
    return NULL;
}

ONYX_DEF(alGetString, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(alGetString(P(0, i32)));
    return NULL;
}

ONYX_DEF(alcGetString, (WASM_I64, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(alcGetString((ALCdevice *) P(0, i64), P(1, i32)));
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
    ONYX_FUNC(alGetString)
    ONYX_FUNC(alcGetString)
    NULL
};