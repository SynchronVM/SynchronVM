#ifndef _PENG_PLATFORM_H
#define _PENG_PLATFORM_H

/* TODO: slot in correct values here */
#define NANOSECOND_TICKS(x)  (x)
#define MICROSECOND_TICKS(x) ((x) *          1000L)
#define MILLISECOND_TICKS(x) ((x) *       1000000L)
#define SECOND_TICKS(x)      ((x) *    1000000000L)
#define MINUTE_TICKS(x)      ((x) *   60000000000L)
#define HOUR_TICKS(x)        ((x) * 3600000000000L)

#endif
