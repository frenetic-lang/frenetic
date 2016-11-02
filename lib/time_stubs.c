#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __MACH__
#include <time.h>
#include <mach/clock.h>
#include <mach/mach.h>
#elif defined __linux__
#include <sys/time.h>
#include <time.h>
#include <errno.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

CAMLprim value frenetic_gettime() {
  CAMLparam0();

  CAMLlocal1(res);

   struct timespec ts;
#ifdef __MACH__
    clock_serv_t cclock;
    mach_timespec_t mts;
    host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
    clock_get_time(cclock, &mts);
    mach_port_deallocate(mach_task_self(), cclock);
    ts.tv_sec = mts.tv_sec;
    ts.tv_nsec = mts.tv_nsec;
#elif defined __linux__
   if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
    switch (errno) {
    case EINVAL:
      /* not supported clkid*/
      caml_invalid_argument ("unsupported clock");
    case EFAULT:
      /* invalid ts, SHOULD NOT HAPPEN*/
    default:
      caml_failwith ("unknown failure");
    }
  }
#endif

  res = copy_int64(ts.tv_sec * 1000000000LL + ts.tv_nsec);

  CAMLreturn(res);
}


