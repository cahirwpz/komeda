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

typedef struct Adsr {
  bool active;
  size_t attack, decay, release;
  float sustain;
} AdsrT;
typedef struct FOF {
  bool active;
  /*non recursive*/
  float b0, b1;
  /*recursive*/
  float a1;
  /* buffer*/
  float x1;
  float y1;
} FOFT;
typedef struct SOF {
  bool active;
    /*non recursive*/
    float b0, b1,b2;
    /*recursive*/
    float a1,a2;
    /* buffer*/
  float x1, x2;
  float y1, y2;
} SOFT;

typedef struct Synth {
  volatile bool active;

  size_t now, end;
  size_t pt, pitch;
  float (*osc)(float);

  float volume;
  AdsrT adsr;
  FOFT lopass;
  SOFT peakEq;
} SynthT;

static SynthT Hardware[HW_SYNTHS];

void SynthVolume(size_t num, float volume) {
  SynthT *synth = &Hardware[num];

  synth->volume = volume;
}

void SynthSetLoPass(size_t num,
                  size_t cutOff)
{
  SynthT *synth = &Hardware[num];

  assert(cutOff > 0.0 && cutOff < SAMPLE_RATE/2);

  float w = tan(M_PI*cutOff/SAMPLE_RATE);
  float n = 1/(1+w);
  synth->lopass.b0 = w*n;
  /*for the low pass filter b0 and b1 are the same*/
  synth->lopass.b1 = synth->lopass.b0;
  synth->lopass.a1 = synth->lopass.b0-n; /*n*(w-1)*/
  synth->lopass.x1=0;
  synth->lopass.y1=0;
  synth->lopass.active=true;
}
void SynthClearLoPass(size_t num) {
  SynthT *synth = &Hardware[num];

  synth->lopass.active = false;
}
void SynthSetPeakEq(size_t num,
                  size_t frequency, float quality, float gain)
{
  SynthT *synth = &Hardware[num];

  assert(frequency > 0.0 && frequency < SAMPLE_RATE/2);

  float a;
  if(gain<1)
  {
	  a = quality;
	  quality=quality*gain;
  }
  else
  {
	  a= quality/gain;
  }
  float w = tan(M_PI*frequency/SAMPLE_RATE);
  float n = 1/(w*w + w/quality +1);
  synth->peakEq.b0 = n*(w*w+w/a +1);
  synth->peakEq.b1 = n*(w*w-1);
  synth->peakEq.b2 = n*(w*w-w/a +1);
  synth->peakEq.a1 = synth->peakEq.b1;
  synth->peakEq.a2 = n*(w*w-w/quality +1);
  synth->peakEq.x1 = 0;
  synth->peakEq.x2 = 0;
  synth->peakEq.y1 = 0;
  synth->peakEq.y2 = 0;
}
void SynthClearPeakEq(size_t num) {
  SynthT *synth = &Hardware[num];

  synth->peakEq.active = false;
}

void SynthSetADSR(size_t num,
                  float attack, float decay, float sustain, float release)
{
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
/*if needed logic can be put into the function computing first order IIR filter
which should get just needed FOFT structure instead of synth*/
static float LowPassFilter(SynthT *synth, float x)
{
	float value;
	value = synth->lopass.b0 *x + synth->lopass.b1 * synth->lopass.x1 + synth->lopass.a1 * synth->lopass.y1;
	synth->lopass.x1=x;
	synth->lopass.y1=value;
	return value;
}

/*if needed logic can be put into the function computing second order IIR filter
which should get just needed SOFT structure instead of synth*/
static float PeakEq(SynthT *synth, float x)
{
	float value;
	value = synth->peakEq.b0 *x + synth->peakEq.b1 * synth->peakEq.x1 + synth->peakEq.b2 * synth->peakEq.x2
			+ synth->peakEq.a1 * synth->peakEq.y1 + synth->peakEq.a2 * synth->peakEq.y2;
	synth->peakEq.x2=synth->peakEq.x1;
	synth->peakEq.x1=x;
	synth->peakEq.y2=synth->peakEq.y1;
	synth->peakEq.y1=value;
	return value;
}

static float ADSR(SynthT *synth) {
  AdsrT *adsr = &synth->adsr;
  size_t t = synth->now;
  size_t t_end = synth->end;

  /* attack? */
  if (t < adsr->attack)
    return (float)t / adsr->attack;

  /* decay? */
  if (t < adsr->decay + adsr->attack) {
    float tr = (float)(t - adsr->attack) / adsr->decay;
    return 1.0 - tr * (1.0 - adsr->sustain);
  }

  /* sustain? */
  if (t < t_end - adsr->release)
    return adsr->sustain;

  /* release? */
  if (t < t_end) {
    float tr = (float)(t_end - t) / adsr->release;
    return adsr->sustain * tr;
  }

  /* no sound! */
  synth->active = false;
  return 0.0;
}

/* should be around 5 to 20 ms */
#define FADE_TIME (SAMPLE_RATE / 100)

static float ASR(SynthT *synth) {
  size_t t = synth->now;
  size_t t_end = synth->end;

  /* attack? */
  if (t < FADE_TIME)
    return (float)t / FADE_TIME;

  /* sustain? */
  if (t < t_end - FADE_TIME)
    return 1.0;

  /* release? */
  if (t < t_end)
    return (float)(t_end - t) / FADE_TIME;

  /* no sound! */
  synth->active = false;
  return 0.0;
}

static float SynthNextSample(size_t num) {
  SynthT *synth = &Hardware[num];

  float v = 0.0;

  if (synth->active) {
    float t = (float)synth->pt / (float)SAMPLE_RATE;

    /*
     * "t" has to be within [0.0, 1.0)
     * "i" is number of samples generated so far
     *
     * We know that:
     * b) synth->pt == i * Pitch
     * c) synth->now == i
     * a) SamplesPerPeriod := SampleRate / Pitch
     *
     * t := synth->pt / SampleRate
     *    = i * Pitch / SampleRate
     *    = i / (SampleRate / Pitch)
     *    = i / SamplesPerPeriod
     */

    v = synth->osc(t) * synth->volume;
    v *= synth->adsr.active ? ADSR(synth) : ASR(synth);
    if(synth->lopass.active)
    {
		v=LowPassFilter(synth,v);
	}
	if(synth->peakEq.active)
	{
			v=PeakEq(synth,v);
	}

    /* By adding the value of a LFO here we can do frequency modulation here. */
    synth->pt += synth->pitch;

    /* Wrap "synth->pt" around so that "t" doesn't go above 1.0 */
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

/**
 * Oscillators.
 *
 * @param t: [0.0, 1.0)
 * @returns: [-1.0, 1.0]
 */

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
  synth->volume = 1.0;
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

    for (j = 0; j < HW_SYNTHS; j++) {
      if (SynthIsActive(j))
        s += SynthNextSample(j);
    }

    s *= 1.0 / HW_SYNTHS;

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
  bzero(&Hardware, sizeof(SynthT) * HW_SYNTHS);

  /* initialize random number generator */
  srand48(time(NULL));

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
