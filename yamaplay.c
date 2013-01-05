#include "SDL.h"
#include "SDL_audio.h"

typedef struct Sound {
  uint32_t frames;
  uint16_t frameRate;
  uint8_t  sample[0];
} SoundT;

typedef struct Track {
  uint8_t instruction[0];
} TrackT;

typedef struct Music {
  uint32_t tracks;
  uint32_t instruments;

  TrackT *track;
  SoundT *instrument;
} MusicT;

MusicT *LoadYama(const char *filename);

int main(int argc, char *argv[]) {
  if (SDL_Init(SDL_INIT_TIMER | SDL_INIT_AUDIO) < 0) {
    fprintf(stderr, "Unable to init SDL: %s\n", SDL_GetError());
    exit(1);
  }

  SDL_AudioSpec fmt;

  fmt.freq = 22050;
  fmt.format = AUDIO_S16;
  fmt.channels = 2;
  fmt.samples = 512;
  fmt.callback = NULL;
  fmt.userdata = NULL;

  /* Open the audio device and start playing sound! */
  if (SDL_OpenAudio(&fmt, NULL) < 0) {
    fprintf(stderr, "Unable to open audio: %s\n", SDL_GetError());
    exit(1);
  }

  SDL_PauseAudio(0);

  SDL_CloseAudio();
  SDL_Quit();

  return 0;
}
