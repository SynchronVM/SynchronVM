#include <stdio.h>
#include <stdarg.h>

void dbg_print(const char *str, ...) {
  va_list args;

  va_start(args, str);
  vprintf(str, args);
  va_end(args);
}
