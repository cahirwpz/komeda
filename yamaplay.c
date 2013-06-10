#include <arpa/inet.h>
#include <assert.h>
#include <math.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <time.h>

#include <portaudio.h>
#include <samplerate.h>

#define SAMPLE_RATE 44100
#define NUM_SECONDS 10

typedef struct Sound {
  size_t frames;
  float  *data;
} SoundT;

typedef struct Channel {
  size_t  commands;
  uint8_t *data;

  bool played;
  int sound;
  int frameNum;
} ChannelT;

typedef struct Player {
  size_t soundNum;
  size_t channelNum;

  SoundT   *sound;
  ChannelT *channel;
} PlayerT;

typedef enum { InstR, OctR, BpmR, PitchR, VolR, BrR, CtrR } RegNumT;

typedef struct Registers {
  uint8_t  instrument;
  uint8_t  octave;
  uint16_t bpm;
  uint8_t  pitch;
  uint8_t  volume;
  uint8_t  *branch;
  uint8_t  counter;
} RegistersT;

/*
 * Raw file handling. Byte ordering is big-endian.
 */

int8_t ReadInt8(FILE *file) {
  int8_t i;
  fread(&i, sizeof(i), 1, file);
  return i;
}

int16_t ReadInt16(FILE *file) {
  int16_t i;
  fread(&i, sizeof(i), 1, file);
  return ntohs(i);
}

int32_t ReadInt32(FILE *file) {
  int32_t i;
  fread(&i, sizeof(i), 1, file);
  return ntohl(i);
}

void ReadBytes(FILE *file, void *data, size_t length) {
  fread(data, length, 1, file);
}

/*
 * Read Yama binary file structures.
 */

void ReadYamaTracks(FILE *file, PlayerT *player) {
  ChannelT *channel;
  int i, num;

  player->channelNum = num = ReadInt16(file);
  player->channel = calloc(num, sizeof(ChannelT));

  channel = player->channel;

  for (i = 0; i < num; i++) {
    int16_t size = ReadInt16(file);
    int16_t commands = ReadInt16(file);

    channel[i].data = malloc(size);
    channel[i].commands = commands;
    ReadBytes(file, channel[i].data, size);

    printf("Track %d: %zd commands (%d) bytes.\n", i, commands, size);
  }
}

void ReadYamaSounds(FILE *file, PlayerT *player) {
  SoundT *sound;
  int i, j, num;

  player->soundNum = num = ReadInt16(file);
  player->sound = calloc(num, sizeof(SoundT));

  sound = player->sound;

  for (i = 0; i < num; i++) {
    int32_t frames = ReadInt32(file);
    int16_t frameRate = ReadInt16(file);

    SRC_DATA resampler;

    resampler.src_ratio = (double)SAMPLE_RATE / (double)frameRate;
    resampler.input_frames = frames;
    resampler.output_frames = (int)(resampler.input_frames * resampler.src_ratio);
    resampler.data_in = malloc(sizeof(float) * resampler.input_frames);
    resampler.data_out = malloc(sizeof(float) * resampler.output_frames);

    for (j = 0; j < frames; j++)
      resampler.data_in[j] = (float)ReadInt8(file) / 128;

    (void)src_simple(&resampler, SRC_SINC_BEST_QUALITY, 1);

    free(resampler.data_in);

    sound[i].frames = resampler.output_frames;
    sound[i].data = resampler.data_out;

    printf("Instrument %d: %zd frames at %dHz (%.3fs)\n", i,
           sound[i].frames, SAMPLE_RATE,
           (float)sound[i].frames / (float)SAMPLE_RATE);
  }
}

PlayerT *LoadYama(const char *path) {
  PlayerT *player = malloc(sizeof(PlayerT));

  FILE *file = fopen(path, "rb");
  ReadYamaTracks(file, player);
  ReadYamaSounds(file, player);
  fclose(file);

  return player;
}

/*
 * Emulate hardware 2-channel direct sound synthesis.
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

#define HW_CHANNELS 2

SynthT Hardware[HW_CHANNELS];

void HardwareInit() {
  bzero(&Hardware, sizeof(Hardware));
}

/*
 * @pitch: in Hertz
 * @length: in seconds
 */
void SynthSet(size_t num, float (*osc)(float), size_t pitch, float length) {
  SynthT *synth = &Hardware[num];

  synth->end = SAMPLE_RATE * length;
  synth->pitch = pitch;
  synth->osc = osc;

  synth->active = false;
  synth->adsr.active = false;
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

void SynthStart(size_t num) {
  SynthT *synth = &Hardware[num];

  synth->now = 0;
  synth->pt = 0;
  synth->active = true;
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

/*
 * Playback routines.
 */

static int PlayYamaCallback(const void *inputBuffer,
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

/*
 * Main program.
 */

void Pa_NoFail(PaError err) {
  if (err != paNoError) {
    fprintf(stderr, "PortAudio error: %s\n", Pa_GetErrorText(err));
    exit(EXIT_FAILURE);
  }
}

static bool ExitRequest = false;

static void SigIntHandler(int signo) {
  ExitRequest = true;
  signal(SIGINT, SIG_DFL);
}

int main(int argc, char *argv[]) {
  HardwareInit();

  /* initialize random number generator */
  srand48(time(NULL));

  /* print some diagnostic messages */
  printf("%s\n", Pa_GetVersionText());

  /* set up keyboard break handler */
  signal(SIGINT, SigIntHandler);

  /* load music file from disk */
  {
    PaStream *stream;
    int i = 0;

    Pa_NoFail(Pa_Initialize());
    Pa_NoFail(Pa_OpenDefaultStream(&stream, 0, 2, paFloat32, SAMPLE_RATE, 256,
                                   PlayYamaCallback, NULL));
    Pa_NoFail(Pa_StartStream(stream));

    while (!ExitRequest) {
      SynthSet(0, Sine, (i & 1) ? 440 : 220, 1.0);
      SynthSetADSR(0, 0.2, 0.2, 0.5, 0.3);
      SynthStart(0);

      SynthSet(1, Square, (i & 1) ? 330 : 660, 0.5);
      SynthStart(1);

      Pa_Sleep(1000);
      i++;
    }

    printf("Quitting%s.\n", ExitRequest ? " by user request" : "");
    Pa_NoFail(Pa_StopStream(stream));
    Pa_NoFail(Pa_CloseStream(stream));
    Pa_NoFail(Pa_Terminate());
  }

  return 0;
}
