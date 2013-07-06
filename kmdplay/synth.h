#ifndef __SYNTH_H__
#define __SYNTH_H__

#include <stddef.h>
#include <stdbool.h>

#define SAMPLE_RATE 44100
#define HW_SYNTHS 2

typedef enum { OSC_SAW, OSC_TRIANGLE, OSC_SINE, OSC_SQUARE, OSC_NOISE } OscT;

void SynthStart();
void SynthStop();

/**
 * @param pitch: in hertz
 * @param lenght: in seconds
 */
void SynthPlay(size_t num, size_t pitch, float length);

/**
 * @param num: oscillator number
 */
void SynthSet(size_t num, OscT osc);
void SynthVolume(size_t num, float volume);
void SynthSetADSR(size_t num,
                  float attack, float decay, float sustain, float release);
void SynthClearADSR(size_t num);
void SynthSetLoPass(size_t num,
                  size_t cutOff);
void SynthClearLoPass(size_t num);
void SynthSetPeakEq(size_t num,
                  size_t frequency, float quality, float gain);
void SynthClearPeakEq(size_t num);

#endif
