/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2022 Abhiroop Sarkar, Robert Krook  				  */
/* 										  */
/* Permission is hereby granted, free of charge, to any person obtaining a copy	  */
/* of this software and associated documentation files (the "Software"), to deal  */
/* in the Software without restriction, including without limitation the rights	  */
/* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell	  */
/* copies of the Software, and to permit persons to whom the Software is	  */
/* furnished to do so, subject to the following conditions:			  */
/* 										  */
/* The above copyright notice and this permission notice shall be included in all */
/* copies or substantial portions of the Software.				  */
/* 										  */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR	  */
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,	  */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE	  */
/* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER	  */
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  */
/* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  */
/* SOFTWARE.									  */
/**********************************************************************************/



#ifndef FFI_UTIL_H
#define FFI_UTIL_H

#include <typedefs.h>
#include <VMC.h>

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



/**util funcs for FFI-HEAP communication ***/
extern cam_value_t cvt_get_fst(cam_value_t *v);
extern cam_value_t cvt_get_snd(cam_value_t *v);
extern void cvt_set_fst(cam_value_t *ptr, cam_value_t *v);
extern void cvt_set_snd(cam_value_t *ptr, cam_value_t *v);
extern void cvt_set(cam_value_t *ptr, cam_value_t *f, cam_value_t *s);
extern bool is_pointer(cam_value_t *v);

/**
 * @brief Allocate a pair on the heap, and get a cam_value_t back that is a pointer to
 * that cell.
 */
extern cam_value_t alloc_cvt();

#endif // FFI_UTIL_H
