#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <pulse/simple.h>
#include <pulse/error.h>

#ifndef __IDRIS_PA_H__
#define __IDRIS_PA_H__

#define IDRIS_PA_BUF_LEN 1024

struct pa_idr {
  pa_simple * pa;
  float buf[IDRIS_PA_BUF_LEN];
  unsigned int bufIdx;
};

struct pa_idr * pa_start_in( char *, char * );
struct pa_idr * pa_start_out( char *, char * );
void pa_stop( struct pa_idr * );

float pa_get( struct pa_idr * );
void pa_put( struct pa_idr *, float );

#endif
