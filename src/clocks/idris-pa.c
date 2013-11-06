#include <stdlib.h>
#include <stdio.h>
#include "idris-pa.h"

struct pa_idr * pa_start_in( char * appName, char * strName) {
  static const pa_sample_spec ss = {
    .format = PA_SAMPLE_FLOAT32LE,
    .rate = 44100,
    .channels = 1
  };

  struct pa_idr * paIdr = malloc(sizeof(struct pa_idr));
  paIdr->pa = pa_simple_new(NULL, appName, PA_STREAM_RECORD, NULL, strName, &ss, NULL, NULL, NULL);
  paIdr->bufIdx = IDRIS_PA_BUF_LEN;
  return paIdr;
}


struct pa_idr * pa_start_out( char * appName, char * strName) {
  static const pa_sample_spec ss = {
    .format = PA_SAMPLE_FLOAT32LE,
    .rate = 44100,
    .channels = 1
  };

  struct pa_idr * paIdr = malloc(sizeof(struct pa_idr));
  paIdr->pa = pa_simple_new(NULL, appName, PA_STREAM_PLAYBACK, NULL, strName, &ss, NULL, NULL, NULL);
  paIdr->bufIdx = 0;
  return paIdr;
}


void pa_stop( struct pa_idr * paIdr ) {
  pa_simple_free(paIdr->pa);
  free(paIdr);
}

float pa_get( struct pa_idr * paIdr) {
  if( paIdr->bufIdx >= IDRIS_PA_BUF_LEN ) {
    int error;
    pa_simple_read( paIdr->pa, paIdr->buf, sizeof(float) * IDRIS_PA_BUF_LEN, &error);
    paIdr->bufIdx = 0;
  }
  float res = paIdr->buf[paIdr->bufIdx];
  ++paIdr->bufIdx;
  return res;
}

void pa_put( struct pa_idr * paIdr, float x) {
  paIdr->buf[paIdr->bufIdx] = x;
  ++paIdr->bufIdx;
  if( paIdr->bufIdx >= IDRIS_PA_BUF_LEN ) {
    int error;
    pa_simple_write( paIdr->pa, paIdr->buf, sizeof(float) * IDRIS_PA_BUF_LEN, &error);
    paIdr->bufIdx = 0;
  }
}

