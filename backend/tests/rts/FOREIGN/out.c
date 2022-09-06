#include "foreign.h"
cam_value_t foreign_prnLst_trampoline(cam_value_t *args) {
    return foreign_prnLst(args[0]);
}

cam_value_t foreign_allocLst_trampoline(cam_value_t *args) {
    return foreign_allocLst(args[0]);
}

