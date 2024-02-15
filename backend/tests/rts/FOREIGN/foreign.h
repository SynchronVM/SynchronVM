#ifndef __FOREIGN_H_
#define __FOREIGN_H_

#include<typedefs.h>
#include<ffi_util.h>

cam_value_t foreign_prnLst(cam_value_t x);
cam_value_t foreign_allocLst(cam_value_t x);
cam_value_t foreign_print_tupleIntIntList(cam_value_t x);
cam_value_t foreign_create_histogram(cam_value_t numbuckets, cam_value_t x);
cam_value_t foreign_record_start_time(cam_value_t x);
cam_value_t foreign_record_end_time(cam_value_t x);
cam_value_t foreign_print_elapsed_time(cam_value_t x);
cam_value_t foreign_print_int(cam_value_t theint);
cam_value_t foreign_print_two_ints(cam_value_t fst, cam_value_t snd);
#endif
