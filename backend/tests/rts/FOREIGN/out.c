#include "foreign.h"
inline cam_value_t foreign_prnLst_trampoline(cam_value_t *args) {
    return foreign_prnLst(args[0]);
}

