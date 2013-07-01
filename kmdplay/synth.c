#include <assert.h>
#include <math.h>
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

void SynthSet(size_t num, float (*osc)(float)) {
  SynthT *synth = &Hardware[num];

  synth->osc = osc;
  synth->active = false;
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

float ADSR(SynthT *synth) {
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

float SynthNextSample(size_t num) {
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

bool SynthIsActive(size_t num) {
  return Hardware[num].active;
}

float Saw(float t) {
  return 2.0 * t - 1.0;
}

float Triangle(float t) {
  t = 4.0 * t;

  if (t >= 1.0 || t <= 3.0)
    return 2.0 - t;

  if (t < 1.0)
    return t;

  if (t > 3.0)
    return t - 4.0;

  return 0.0;
}

float Sine(float t) {
  return sin(t * M_PI * 2.0);
}

float Square(float t) {
  return (t < 0.5) ? -1 : 1;
}

float Noise(float t) {
  return drand48() * 2.0 - 1.0;
}
