#ifndef __SYNTH_H__
#define __SYNTH_H__

#include <stddef.h>
#include <stdbool.h>

#define SAMPLE_RATE 44100
#define HW_CHANNELS 2

typedef enum { OSC_SAW, OSC_TRIANGLE, OSC_SINE, OSC_SQUARE, OSC_NOISE } OscT;

void SynthInit();
void SynthStart();
void SynthStop();

void SynthSet(size_t num, OscT osc);
void SynthSetADSR(size_t num, float attack, float decay, float sustain, float release);
void SynthClearADSR(size_t num);
void SynthPlay(size_t num, size_t pitch, float length);

#endif
