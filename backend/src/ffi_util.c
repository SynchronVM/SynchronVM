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



cam_value_t cvt_fst(cam_value_t *v){
  return heap_fst(&vmc->heap, (heap_index)v->value);
}

cam_value_t cvt_snd(cam_value_t *v){
  return heap_snd(&vmc->heap, (heap_index)v->value);
}

bool is_pointer(cam_value_t *v){
  return (v->flags == 32768); // PTR is 0x8000 (32768)
}
