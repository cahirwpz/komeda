#include "debug.h"
#include "player.h"
#include "synth.h"

ChannelT channel0 = { 0, {{440, 1}, {0, 0.5}, {660, 1.5}, {330, 2}, {0, 0}} };
ChannelT channel1 = { 0, {{800, 1}, {600, 1}, {800, 1}, {600, 1}, {800, 1}, {0, 0}} };

int main(int argc, char *argv[]) {
  PlayerInit(HW_SYNTHS, &channel0, &channel1);

  SynthStart();
  SynthSet(0, OSC_SINE);
  SynthSetADSR(0, 0.2,1.0, 0.2, 0.5, 0.3);
  SynthSet(1, OSC_SQUARE);

  bool success = PlayerRun();

  printf("Quitting%s.\n", success ? "" : " by user request");
  SynthStop();

  return 0;
}
