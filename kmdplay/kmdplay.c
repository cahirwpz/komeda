#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "debug.h"
#include "synth.h"

typedef struct timeval timeval_t;

#define NUM_SECONDS 10

static volatile bool ExitRequest = false;

/*
 * Simple channel handling.
 */

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
 * Main program.
 */

static void SigIntHandler(int signo) {
  ExitRequest = true;
  signal(SIGINT, SIG_DFL);
}

ChannelT channel0 = { 0, {{440, 1}, {0, 0.5}, {660, 1.5}, {330, 2}, {0, 0}} };
ChannelT channel1 = { 0, {{800, 1}, {600, 1}, {800, 1}, {600, 1}, {800, 1}, {0, 0}} };

int main(int argc, char *argv[]) {
  SynthInit();

  /* set up keyboard break handler */
  signal(SIGINT, SigIntHandler);

  /* load music file from disk */
  SynthStart();

  {
    PlayerT player;
    PlayerInit(&player, HW_CHANNELS, &channel0, &channel1);

    SynthSet(0, OSC_SINE);
    SynthSetADSR(0, 0.2, 0.2, 0.5, 0.3);
    SynthSet(1, OSC_SQUARE);

    PlayerRun(&player);
  }

  printf("Quitting%s.\n", ExitRequest ? " by user request" : "");
  SynthStop();

  return 0;
}
