#include "foreign.h"
cam_value_t foreign_add_trampoline(cam_value_t *args) {
    return foreign_add(args[0], args[1]);
}

