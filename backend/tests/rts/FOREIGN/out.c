#include "foreign.h"
cam_value_t foreign_print_tupleIntIntList_trampoline(cam_value_t *args) {
    return foreign_print_tupleIntIntList(args[0]);
}

