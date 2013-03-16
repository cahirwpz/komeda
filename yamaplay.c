#include <arpa/inet.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

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
 * Playback routines.
 */

static int PlayYamaCallback(const void *inputBuffer,
                            void *outputBuffer,
                            unsigned long framesPerBuffer,
                            const PaStreamCallbackTimeInfo *timeInfo,
                            PaStreamCallbackFlags statusFlags,
                            void *userData)
{
  PlayerT *player = (PlayerT *)userData;
  float *out = (float*)outputBuffer;
  size_t i;

  for (i = 0; i < framesPerBuffer; i++) {
    ChannelT *channel = &player->channel[0];
    SoundT *sound = &player->sound[0];

    if (channel->played) {
      int n = (channel->frameNum + i) % sound->frames;

      *out++ = sound->data[n]; /* left */
      *out++ = sound->data[n]; /* right */
    } else {
      *out++ = 0.0f;  /* left */
      *out++ = 0.0f;  /* right */
    }
  }

  player->channel[0].frameNum =
      (player->channel[0].frameNum + framesPerBuffer) % player->sound[0].frames;

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
  PlayerT *player = NULL;

  /* print some diagnostic messages */
  printf("%s\n", Pa_GetVersionText());

  /* set up keyboard break handler */
  signal(SIGINT, SigIntHandler);

  /* load music file from disk */
  if (argc > 1)
    player = LoadYama(argv[1]); 

  if (player) {
    PaStream *stream;

    player->channel[0].played = true;

    Pa_NoFail(Pa_Initialize());
    Pa_NoFail(Pa_OpenDefaultStream(&stream, 0, 2, paFloat32, SAMPLE_RATE, 256,
                                   PlayYamaCallback, (void *)player));
    Pa_NoFail(Pa_StartStream(stream));
    Pa_Sleep(NUM_SECONDS * 1000);
    printf("Quitting%s.\n", ExitRequest ? " by user request" : "");
    Pa_NoFail(Pa_StopStream(stream));
    Pa_NoFail(Pa_CloseStream(stream));
    Pa_NoFail(Pa_Terminate());
  }

  return 0;
}
