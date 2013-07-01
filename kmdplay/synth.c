#include <assert.h>
#include <math.h>
#include <portaudio.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <sys/time.h>

#include "synth.h"

/*
 * Emulate hardware N-channel direct sound synthesis.
 */
typedef struct Synth {
  volatile bool active;

  size_t now, end;
  size_t pt, pitch;
  float (*osc)(float);

  struct {
    bool active;
    size_t attack, decay, release;
    float sustain;
  } adsr;
} SynthT;

static SynthT Hardware[HW_CHANNELS];

void SynthInit() {
  bzero(&Hardware, sizeof(SynthT) * HW_CHANNELS);

  /* initialize random number generator */
  srand48(time(NULL));
}

void SynthSetADSR(size_t num, float attack, float decay, float sustain, float release) {
  SynthT *synth = &Hardware[num];

  assert(sustain > 0.0 && sustain < 1.0);

  synth->adsr.attack = attack * SAMPLE_RATE;
  synth->adsr.decay = decay * SAMPLE_RATE;
  synth->adsr.sustain = sustain;
  synth->adsr.release = release * SAMPLE_RATE;
  synth->adsr.active = true;
}

void SynthClearADSR(size_t num) {
  SynthT *synth = &Hardware[num];

  synth->adsr.active = false;
}

void SynthPlay(size_t num, size_t pitch, float length) {
  SynthT *synth = &Hardware[num];

  synth->now = 0;
  synth->pt = 0;
  synth->active = true;
  synth->end = SAMPLE_RATE * length;
  synth->pitch = pitch;
}

static float ADSR(SynthT *synth) {
  size_t t = synth->now;

  /* attack? */
  if (t < synth->adsr.attack)
    return (float)t / synth->adsr.attack;

  t -= synth->adsr.attack;

  /* decay? */
  if (t < synth->adsr.decay) {
    float tr = (float)t / synth->adsr.decay;
    return tr * (synth->adsr.sustain - 1.0) + 1.0;
  }

  /* sustain? */
  if (synth->now < synth->end)
    return synth->adsr.sustain;

  /* release? */
  t = synth->now - synth->end;

  if (t < synth->adsr.release) {
    float tr = (float)t / synth->adsr.release;
    return synth->adsr.sustain * (1.0 - tr);
  }

  /* no sound! */
  synth->active = false;
  return 0.0;
}

static float SynthNextSample(size_t num) {
  SynthT *synth = &Hardware[num];

  float v = 0.0;

  if (synth->active) {
    float pt = synth->pt / (float)SAMPLE_RATE;

    v = synth->osc(pt);

    if (synth->adsr.active) {
      v *= ADSR(synth);
    } else if (synth->now >= synth->end) {
      synth->active = false;
    }

    synth->pt += synth->pitch;

    if (synth->pt > SAMPLE_RATE)
      synth->pt -= SAMPLE_RATE;

    synth->now++;
  } else {
    synth->active = false;
  }

  return v;
}

static bool SynthIsActive(size_t num) {
  return Hardware[num].active;
}

static float Saw(float t) {
  return 2.0 * t - 1.0;
}

static float Triangle(float t) {
  t = 4.0 * t;

  if (t >= 1.0 || t <= 3.0)
    return 2.0 - t;

  if (t < 1.0)
    return t;

  if (t > 3.0)
    return t - 4.0;

  return 0.0;
}

static float Sine(float t) {
  return sin(t * M_PI * 2.0);
}

static float Square(float t) {
  return (t < 0.5) ? -1 : 1;
}

static float Noise(float t) {
  return drand48() * 2.0 - 1.0;
}

static float (*Oscillators[])(float) = {
  &Saw, &Triangle, &Sine, &Square, &Noise
};

void SynthSet(size_t num, OscT osc) {
  SynthT *synth = &Hardware[num];

  synth->osc = Oscillators[osc];
  synth->active = false;
}

/*
 * Playback routines.
 */

static int PlayCallback(const void *inputBuffer,
                        void *outputBuffer,
                        unsigned long framesPerBuffer,
                        const PaStreamCallbackTimeInfo *timeInfo,
                        PaStreamCallbackFlags statusFlags,
                        void *userData)
{
  float *out = (float*)outputBuffer;
  size_t i, j;

  for (i = 0; i < framesPerBuffer; i++) {
    float s = 0.0;

    for (j = 0; j < HW_CHANNELS; j++) {
      if (SynthIsActive(j))
        s += SynthNextSample(j);
    }

    s *= 1.0 / HW_CHANNELS;
      
    *out++ = s; /* left */
    *out++ = s; /* right */
  }

  return 0;
}

static PaStream *stream = NULL;

static void Pa_NoFail(PaError err) {
  if (err != paNoError) {
    fprintf(stderr, "PortAudio error: %s\n", Pa_GetErrorText(err));
    exit(EXIT_FAILURE);
  }
}

void SynthStart() {
  /* print some diagnostic messages */
  fprintf(stderr, "%s\n", Pa_GetVersionText());

  Pa_NoFail(Pa_Initialize());
  Pa_NoFail(Pa_OpenDefaultStream(&stream, 0, 2, paFloat32, SAMPLE_RATE, 256,
                                 PlayCallback, NULL));
  Pa_NoFail(Pa_StartStream(stream));
}

void SynthStop() {
  Pa_NoFail(Pa_StopStream(stream));
  Pa_NoFail(Pa_CloseStream(stream));
  Pa_NoFail(Pa_Terminate());
}
