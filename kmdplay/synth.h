#ifndef __SYNTH_H__
#define __SYNTH_H__

#include <stddef.h>
#include <stdbool.h>

#define SAMPLE_RATE 44100
#define HW_CHANNELS 2

void SynthInit();
void SynthSet(size_t num, float (*osc)(float));
void SynthSetADSR(size_t num, float attack, float decay, float sustain, float release);
void SynthClearADSR(size_t num);
void SynthPlay(size_t num, size_t pitch, float length);
float SynthNextSample(size_t num);
bool SynthIsActive(size_t num);
float Saw(float t);
float Triangle(float t);
float Sine(float t);
float Square(float t);
float Noise(float t);

#endif
