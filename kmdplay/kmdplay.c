#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include <portaudio.h>
#include <samplerate.h>

#include "synth.h"

typedef struct timeval timeval_t;

#define NUM_SECONDS 10

#ifndef NDEBUG
#define DEBUG(fmt, ...) fprintf(stderr, fmt "\n", __VA_ARGS__)
#else
#define DEBUG(fmt, ...)
#endif

static volatile bool ExitRequest = false;

#if 0
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
#endif

/*
 * Raw file handling. Byte ordering is big-endian.
 */

#if 0
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
#endif

/*
 * Read Yama binary file structures.
 */

#if 0
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
#endif

/*
 * Simple channel handling.
 */

typedef struct Program {
  uint16_t cmds[0];
} ProgramT;

typedef struct MachineState {
} MachineStateT;

typedef struct Module {
  uint8_t attr[8];
  void ((*method[8])(void));
} ModuleT;

#define STACK_LENGTH 32

typedef struct ChannelState {
  uint8_t regs[8];
  ModuleT (*mods)[8];
  struct {             /* status register */
    uint8_t slurr : 1; /* slurr mode */
    uint8_t unit  : 3; /* unit length */
    uint8_t plus  : 1; /* a - b > 0 */
    uint8_t zero  : 1; /* a - b = 0 */
  } sr;
  uint8_t pc;  /* program counter */
  uint8_t sp;  /* stack pointer */
  uint8_t aor; /* active object register */
  uint8_t pnr; /* procedure number register */
  uint8_t vnr; /* voice number register */
  uint16_t *cmd;
  uint8_t stack[STACK_LENGTH];
} ChannelStateT;

typedef enum { CMD_NONE, CMD_HALT, CMD_REST, CMD_PLAY } CommandTypeT;

typedef struct Command {
  CommandTypeT type;
  union {
    struct {
      uint8_t n, d;
    } rest;
    struct {
      uint8_t n, d;
      uint8_t pitch;
    } play;
  };
} CommandT;

static inline uint8_t ChannelGetReg(ChannelStateT *channel, uint8_t reg_no) {
  return 0;
}

static inline void ChannelSetReg(ChannelStateT *channel,
                                 uint8_t reg_no, uint8_t value)
{
}

void InterpreterOneStep(MachineStateT *state, ChannelStateT *channel,
                        CommandT *command)
{
  uint8_t u = ((uint8_t *)channel->cmd)[0];
  uint8_t v = ((uint8_t *)channel->cmd)[1];
  uint8_t n = u & 15;
  uint8_t x = v >> 4;
  uint8_t y = v & 15;

  channel->cmd++;
  channel->pc++;
  command->type = CMD_NONE;

  if (u < 0x80) {
    /* NOTE: 1ppp pppp nnnn dddd */
    /* play & rest fields overlap */
    command->play.n = x + 1;
    command->play.d = y + 1;

    if (u < 96) {
      command->type = CMD_PLAY;
      command->play.pitch = u;
    } else if (u == 96) {
      command->type = CMD_REST;
    } else {
      DEBUG("Illegal pitch value: %d!", u);
    }
    return;
  } 
  
  /* Handle A-form. */
  if (u < 0xc0) {
    u = (u >> 4) & 7;

    switch (u) {
      case 0: /* LD #v, Rn */
        ChannelSetReg(channel, n, v);
        break;

      case 1: /* CMOV n:CRx, Ry */
        ChannelSetReg(channel, y, channel->mods[n]->attr[x]);
        break;

      case 2: /* CMOV Rx, n:CRy */
        channel->mods[n]->attr[y] = ChannelGetReg(channel, x);
        break;

      case 3: /* CEXG n:CRx, Ry */
        {
          uint8_t tmp = ChannelGetReg(channel, y);
          ChannelSetReg(channel, y, channel->mods[n]->attr[x]);
          channel->mods[n]->attr[x] = tmp;
        }
        break;

      case 4: /* A-form: LOOP Rn, $v */
        break;

      default: /* A-form: reserved */
        DEBUG("Illegal command: A-form %d!", u);
        break;
    }
    return;
  }
 
  /* Handle B-form. */
  if (u < 0xff) {
    u -= 0xc0;

    switch (u) {
      case 0: /* CALL $v */
        break;
      case 1: /* JMP $v */
        break;
      case 2: /* JEQ $v */
        break;
      case 3: /* JNE $v */
        break;
      case 4: /* JLE $v */
        break;
      case 5: /* JLT $v */
        break;
      case 6: /* JGE $v */
        break;
      case 7: /* JGT $v */
        break;
      case 8: /* MOV Rx, Ry */
        break;
      case 9: /* EXG Rx, Ry */
        break;
      case 10: /* ADD Rx, Ry */
        break;
      case 11: /* SUB Rx, Ry */
        break;
      case 12: /* PRC Rx, Ry */
        break;
      case 13: /* CMP Rx, Ry */
        break;
      case 14: /* PUSHM ~v */
        break;
      case 15: /* POPM ~v */
        break;
      case 16: /* CPUSHM ~v */
        break;
      case 17: /* CPOPM ~v */
        break;
      case 29: /* NOTE Rx:Ry */
        break;
      case 30: /* INVOKE v */
        break;
      default:
        DEBUG("Illegal command: B-form %d!", n); 
        break;
    }
    return;
  }
 
  /* Handle C-form. */
  if (x < 15) {
    switch (x) {
      case 0: /* REST Ry */
        break;
      case 1: /* SIGNAL y */
        break;
      case 2: /* SETUNIT y */
        break;
      case 3: /* ACTIVATE y */
        break;
      default:
        DEBUG("Illegal command: C-form %d!", x);
        break;
    }
    return;
  }
 
  /* Handle D-form. */
  switch (y) {
    case 0: /* NOP */
      break;
    case 1: /* RET */
      break;
    case 2: /* CLRSIG */
      break;
    case 3: /* CHKSIG */
      break;
    case 4: /* WAITSIG */
      break;
    case 5: /* SLURR ON */
      break;
    case 6: /* SLURR OFF */
      break;
    case 15: /* HALT */
      break;
    default:
      DEBUG("Illegal command: D-form %d!", y);
      break;
  }
}

typedef struct Note {
  size_t pitch;  /* in hertz, 0 => rest*/
  float  length; /* in seconds, 0 => end */
} NoteT;

typedef struct Channel {
  size_t current;
  NoteT note[];
} ChannelT;

NoteT *ChannelNextNote(ChannelT *channel) {
  NoteT *note = &channel->note[channel->current];

  if (note->length == 0.0)
    return NULL;

  channel->current++;
  return note;
}

/*
 * Blah.
 */

typedef enum { EV_IGNORE = 0, EV_FETCH, EV_WAIT, EV_LAST } EventTypeT;

typedef struct Event {
  EventTypeT type;
  ssize_t channel;

  union {
    struct {
      timeval_t start;
      timeval_t stop;
    } wait;
  };
} EventT;

#define MAX_EVENT_NUM (HW_CHANNELS * 2)

typedef struct Player {
  ChannelT *channel[HW_CHANNELS];
  EventT queue[MAX_EVENT_NUM];
} PlayerT;

void PlayerInit(PlayerT *player, size_t channels, ...) {
  va_list ap;
  int i;

  bzero(player, sizeof(PlayerT));

  va_start(ap, channels);

  for (i = 0; i < channels; i++) {
    player->channel[i] = va_arg(ap, ChannelT *);

    player->queue[i].type = EV_FETCH;
    player->queue[i].channel = i;
  }

  va_end(ap);
}

/*
 * Returns the ''timeval'' when next event should be scheduled.  If returns
 * false then no further events are expected.
 */
bool PlayerOneStep(PlayerT *player, timeval_t *wakeup) {
  /*
   * This routine doesn't use any O(log n) data structure, because the queue is
   * really short.
   */
  timeval_t now;
  int i;

  gettimeofday(&now, NULL);

  /* clean up expired waits */
  for (i = 0; i < MAX_EVENT_NUM; i++) {
    EventT *event = &player->queue[i];

    if (event->type == EV_WAIT)
      if (timercmp(&event->wait.stop, &now, <)) {
        event->type = EV_FETCH;
        DEBUG("ch %zd: wait => fetch", event->channel);
      }
  }

  /* fetch data from channels */
  for (i = 0; i < MAX_EVENT_NUM; i++) {
    EventT *event = &player->queue[i];

    if (event->type == EV_FETCH) {
      NoteT *note = ChannelNextNote(player->channel[event->channel]);

      if (note) {
        if (note->pitch) {
          DEBUG("ch %zd: play (%zd, %f)", event->channel, note->pitch, note->length);
          SynthPlay(event->channel, note->pitch, note->length);
        } else {
          DEBUG("ch %zd: rest (%f)", event->channel, note->length);
        }

        float l_int = trunc(note->length);
        float l_frac = note->length - l_int;
        struct timeval length = { (int)l_int, (int)(l_frac * 1000000) };

        event->type = EV_WAIT;
        memcpy(&event->wait.start, &now, sizeof(timeval_t));
        timeradd(&now, &length, &event->wait.stop);
      } else {
        DEBUG("ch %zd: finished", event->channel);
        event->type = EV_IGNORE;
      }
    }
  }

  /* set up timer */
  bool found_next = false;

  memcpy(wakeup, &now, sizeof(timeval_t));
  wakeup->tv_sec += 60*60*24*365;  /* one year is large enough ;-) */

  for (i = 0; i < MAX_EVENT_NUM; i++) {
    EventT *event = &player->queue[i];

    if (event->type == EV_WAIT) {
      if (timercmp(&event->wait.stop, wakeup, <)) {
        memcpy(wakeup, &event->wait.stop, sizeof(timeval_t));
        found_next = true;
      }
    }
  }

  return found_next;
}

void PlayerRun(PlayerT *player) {
  struct timeval now, wakeup;

  while (!ExitRequest && PlayerOneStep(player, &wakeup)) {
    timeval_t interval;

    bzero(&interval, sizeof(interval));

    gettimeofday(&now, NULL);
    timersub(&wakeup, &now, &interval);

    DEBUG("wake up in : %ld:%.6d", interval.tv_sec, interval.tv_usec);

    while (select(0, NULL, NULL, NULL, &interval) == -1) {
      DEBUG("Spurious wake up: %s!", strerror(errno));
    }
  }
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

static void SigIntHandler(int signo) {
  ExitRequest = true;
  signal(SIGINT, SIG_DFL);
}

ChannelT channel0 = { 0, {{440, 1}, {0, 0.5}, {660, 1.5}, {330, 2}, {0, 0}} };
ChannelT channel1 = { 0, {{800, 1}, {600, 1}, {800, 1}, {600, 1}, {800, 1}, {0, 0}} };

int main(int argc, char *argv[]) {
  SynthInit();

  /* print some diagnostic messages */
  printf("%s\n", Pa_GetVersionText());

  /* set up keyboard break handler */
  signal(SIGINT, SigIntHandler);

  /* load music file from disk */
  {
    PaStream *stream;

    Pa_NoFail(Pa_Initialize());
    Pa_NoFail(Pa_OpenDefaultStream(&stream, 0, 2, paFloat32, SAMPLE_RATE, 256,
                                   PlayYamaCallback, NULL));
    Pa_NoFail(Pa_StartStream(stream));

    {
      PlayerT player;
      PlayerInit(&player, HW_CHANNELS, &channel0, &channel1);

      SynthSet(0, Sine);
      SynthSetADSR(0, 0.2, 0.2, 0.5, 0.3);
      SynthSet(1, Square);

      PlayerRun(&player);
    }

    printf("Quitting%s.\n", ExitRequest ? " by user request" : "");
    Pa_NoFail(Pa_StopStream(stream));
    Pa_NoFail(Pa_CloseStream(stream));
    Pa_NoFail(Pa_Terminate());
  }

  return 0;
}
