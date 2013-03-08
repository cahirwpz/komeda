#include <arpa/inet.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "portaudio.h"
#include "samplerate.h"
#include "sndfile.h"

typedef struct File {
  uint32_t length;
  uint8_t  data[0];
} FileT;

typedef struct Sound {
  uint32_t frames;
  uint16_t frameRate;
  uint8_t  sample[0];
} SoundT;

typedef struct Track {
  uint8_t instruction[0];
} TrackT;

typedef struct SoundTable {
  uint32_t num;
  SoundT *sound[0];
} SoundTableT;

typedef struct TrackTable {
  uint32_t num;
  TrackT *track[0];
} TrackTableT;

typedef struct Music {
  SoundTableT *sounds;
  TrackTableT *tracks;
} MusicT;

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

FileT *LoadFile(const char *path) {
  FILE *fh = fopen(path, "rb");
  FileT *file;
  int32_t length;

  fread(&length, sizeof(length), 1, fh);
  length = ntohl(length);

  file = malloc(sizeof(FileT) + length);
  file->length = length;

  fread(file->data, length, 1, fh);
  fclose(fh);

  return file;
}

MusicT *LoadYama(const char *path) {
  FileT *file = LoadFile(path);
  MusicT *music = malloc(sizeof(MusicT));

  uint32_t *offsets = (uint32_t *)file->data;
  int i, num;
  
  {
    TrackT **track;

    num = ntohl(*offsets++);

    music->tracks = malloc(sizeof(TrackTableT) + sizeof(TrackT *) * num);
    music->tracks->num = num;

    track = music->tracks->track;

    for (i = 0; i < num; i++) {
      uint32_t offset = ntohl(*offsets++);

      track[i] = (TrackT *)&file->data[offset];

      printf("track %d at %p (offset %d)\n", i, track[i], offset);
    }
  }

  {
    SoundT **sound;

    num = ntohl(*offsets++);

    music->sounds = malloc(sizeof(SoundTableT) + sizeof(SoundT *) * num);
    music->sounds->num = num;

    sound = music->sounds->sound;

    for (i = 0; i < num; i++) {
      uint32_t offset = ntohl(*offsets++);

      sound[i] = (SoundT *)&file->data[offset];
      sound[i]->frames = ntohl(sound[i]->frames);
      sound[i]->frameRate = ntohs(sound[i]->frameRate);

      printf("instrument %d at %p (offset %d)\n", i, sound[i], offset);
      printf("%d frames at %dHz\n", sound[i]->frames, sound[i]->frameRate);
    }
  }

  return music;
}

void Pa_NoFail(PaError err) {
  if (err != paNoError) {
    fprintf(stderr, "PortAudio error: %s\n", Pa_GetErrorText(err));
    exit(EXIT_FAILURE);
  }
}

int main(int argc, char *argv[]) {
  Pa_NoFail(Pa_Initialize());

  if (argc > 1)
    LoadYama(argv[1]);

  Pa_NoFail(Pa_Terminate());

  return 0;
}
