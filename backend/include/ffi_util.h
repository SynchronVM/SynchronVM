#ifndef FFI_UTIL_H
#define FFI_UTIL_H

#include "typedefs.h"

/* Convert a cam_value_t to a int */
#define cvtToInt(value) ((int) value.value)

/* Convert a cam_value_t to a bool */
#define cvtToBool(value) ((bool) value.value)

/* Write an int value to a cam_value_t */
inline void intToCvt(int x, cam_value_t *dest) {
    dest->value = (UINT)x;
}

/* Write an int value to a cam_value_t */
inline void boolToCvt(bool x, cam_value_t *dest) {
    dest->value = (bool)x;
}

/* cam_value_t representing the camiot "()" value */
cam_value_t unit = { .flags = 0
                   , .value = 0 }

#endif // FFI_UTIL_H
