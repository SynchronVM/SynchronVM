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


#include <ffi_util.h>

static vmc_t *vmc;

// currently assummes FFI with a single container
void init_ffi_container(vmc_t *container){
  vmc = container;
}

// The following functions were not `inline`d because of C99 6.7.4
/*
 * An inline definition of a function with external linkage shall not contain a
 * definition of a modifiable object with static storage duration, and shall not
 * contain a reference to an identifier with internal linkage.
 */



cam_value_t cvt_get_fst(cam_value_t *v){
  return heap_fst(&vmc->heap, (heap_index)v->value);
}

cam_value_t cvt_get_snd(cam_value_t *v){
  return heap_snd(&vmc->heap, (heap_index)v->value);
}

void cvt_set_fst(cam_value_t *ptr, cam_value_t *v) {
  heap_set_fst(&vmc->heap, (heap_index)ptr->value, *v);
}

void cvt_set_snd(cam_value_t *ptr, cam_value_t *v) {
  heap_set_snd(&vmc->heap, (heap_index)ptr->value, *v);
}

void cvt_set(cam_value_t *ptr, cam_value_t *f, cam_value_t *s) {
  cvt_set_fst(ptr, f);
  cvt_set_snd(ptr, s);
}

bool is_pointer(cam_value_t *v){
  return (v->flags == 32768); // PTR is 0x8000 (32768)
}

cam_value_t alloc_cvt(int num_args, ...) {
  cam_value_t ffi_val_arr[num_args];
  va_list args;
  va_start(args, num_args);
  for(int i = 0; i < num_args; i++){
    cam_value_t ffi_val = va_arg(args, cam_value_t);
    ffi_val_arr[i] = ffi_val;
  }
  va_end(args);
  heap_index hi = heap_alloc_FFI_GC(vmc, num_args, ffi_val_arr);
  return (cam_value_t) { .value = hi, .flags = VALUE_PTR_BIT};
}
