#include "foreign.h"
cam_value_t foreign_print_elapsed_time_trampoline(cam_value_t *args) {
    return foreign_print_elapsed_time(args[0]);
}

cam_value_t foreign_print_tupleIntIntList_trampoline(cam_value_t *args) {
    return foreign_print_tupleIntIntList(args[0]);
}

cam_value_t foreign_record_end_time_trampoline(cam_value_t *args) {
    return foreign_record_end_time(args[0]);
}

cam_value_t foreign_create_histogram_trampoline(cam_value_t *args) {
    return foreign_create_histogram(args[0], args[1]);
}

cam_value_t foreign_record_start_time_trampoline(cam_value_t *args) {
    return foreign_record_start_time(args[0]);
}

cam_value_t foreign_print_int_trampoline(cam_value_t *args) {
    return foreign_print_int(args[0]);
}

