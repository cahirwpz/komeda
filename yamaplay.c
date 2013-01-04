#include "SDL.h"
#include "SDL_audio.h"

int main(int argc, char *argv[]) {
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

  return 0;
}
