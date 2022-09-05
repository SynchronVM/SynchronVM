#ifndef TRAMPOLINES_H
#define TRAMPOLINES_H
#include "typedefs.h"
cam_value_t foreign_prnLst_trampoline(cam_value_t *args);
cam_value_t foreign_allocLst_trampoline(cam_value_t *args);
cam_value_t foreign_create_histogram_trampoline(cam_value_t *args);
#endif // TRAMPOLINES_H
