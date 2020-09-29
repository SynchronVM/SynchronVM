/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Joel Svensson, Abhiroop Sarkar             				  */
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
#ifdef DEBUG
#include <stdio.h>
# define DEBUG_PRINT(x) printf x
#else
# define DEBUG_PRINT(x) do {} while (0)
#endif

#include <CAM.h>
#include <VMC.h>
#include <string.h>

/* Return type of an eval function indicates the number of */
/* bytes that were read from the bc_rest array. In case of */
/* error executing a function, it would return -1 */

typedef int (*eval_fun) (vmc_t *vmc, uint8_t *bc_rest);

int eval_fst(vmc_t *vmc, uint8_t *bc_rest);
int eval_snd(vmc_t *vmc, uint8_t *bc_rest);
int eval_acc(vmc_t *vmc, uint8_t *bc_rest);
int eval_rest(vmc_t *vmc, uint8_t *bc_rest);
int eval_push(vmc_t *vmc, uint8_t *bc_rest);
int eval_swap(vmc_t *vmc, uint8_t *bc_rest);
int eval_loadi(vmc_t *vmc, uint8_t *bc_rest);
int eval_loadb(vmc_t *vmc, uint8_t *bc_rest);
int eval_clear(vmc_t *vmc, uint8_t *bc_rest);
int eval_cons(vmc_t *vmc, uint8_t *bc_rest);
int eval_cur(vmc_t *vmc, uint8_t *bc_rest);
int eval_pack(vmc_t *vmc, uint8_t *bc_rest);
int eval_skip(vmc_t *vmc, uint8_t *bc_rest);
int eval_stop(vmc_t *vmc, uint8_t *bc_rest);
int eval_app(vmc_t *vmc, uint8_t *bc_rest);
int eval_return(vmc_t *vmc, uint8_t *bc_rest);
int eval_call(vmc_t *vmc, uint8_t *bc_rest);
int eval_goto(vmc_t *vmc, uint8_t *bc_rest);
int eval_gotofalse(vmc_t *vmc, uint8_t *bc_rest);
int eval_switch(vmc_t *vmc, uint8_t *bc_rest);
int eval_abs(vmc_t *vmc, uint8_t *bc_rest);
int eval_neg(vmc_t *vmc, uint8_t *bc_rest);
int eval_not(vmc_t *vmc, uint8_t *bc_rest);
int eval_dec(vmc_t *vmc, uint8_t *bc_rest);
int eval_add_unsignedi(vmc_t *vmc, uint8_t *bc_rest);
int eval_mul_unsignedi(vmc_t *vmc, uint8_t *bc_rest);
int eval_min_unsignedi(vmc_t *vmc, uint8_t *bc_rest);
int eval_add_signedi(vmc_t *vmc, uint8_t *bc_rest);
int eval_mul_signedi(vmc_t *vmc, uint8_t *bc_rest);
int eval_min_signedi(vmc_t *vmc, uint8_t *bc_rest);
int eval_addf(vmc_t *vmc, uint8_t *bc_rest);
int eval_mulf(vmc_t *vmc, uint8_t *bc_rest);
int eval_minf(vmc_t *vmc, uint8_t *bc_rest);
int eval_gt(vmc_t *vmc, uint8_t *bc_rest);
int eval_lt(vmc_t *vmc, uint8_t *bc_rest);
int eval_eq(vmc_t *vmc, uint8_t *bc_rest);
int eval_ge(vmc_t *vmc, uint8_t *bc_rest);
int eval_le(vmc_t *vmc, uint8_t *bc_rest);

eval_fun evaluators[] =
  { eval_fst,
    eval_snd,
    eval_acc,
    eval_rest,
    eval_push,
    eval_swap,
    eval_loadi,
    eval_loadb,
    eval_clear,
    eval_cons,
    eval_cur,
    eval_pack,
    eval_skip,
    eval_stop,
    eval_app,
    eval_return,
    eval_call,
    eval_goto,
    eval_gotofalse,
    eval_switch,
    eval_abs,
    eval_neg,
    eval_not,
    eval_dec,
    eval_add_unsignedi,
    eval_mul_unsignedi,
    eval_min_unsignedi,
    eval_add_signedi,
    eval_mul_signedi,
    eval_min_signedi,
    eval_addf,
    eval_mulf,
    eval_minf,
    eval_gt,
    eval_lt,
    eval_eq,
    eval_ge,
    eval_le };


int eval_fst(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_value_t v = heap_fst(&vmc->heap, (heap_index)e.value);
  vmc->vm.env = v;
  return 1;
}

int eval_snd(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_value_t v = heap_snd(&vmc->heap, (heap_index)e.value);
  vmc->vm.env = v;
  return 1;
}


int eval_acc(vmc_t *vmc, uint8_t *bc_rest) {
  uint8_t acc_n = bc_rest[0];
  for(unsigned int i = 0; i < acc_n; i++){
    cam_register_t e = vmc->vm.env;
    cam_value_t v = heap_fst(&vmc->heap, (heap_index)e.value);
    vmc->vm.env = v;
  }
  cam_value_t v = heap_snd(&vmc->heap, (heap_index)vmc->vm.env.value);
  vmc->vm.env = v;

  return 2;
}

int eval_rest(vmc_t *vmc, uint8_t *bc_rest)  {
  uint8_t acc_n = bc_rest[0];
  for(unsigned int i = 0; i < acc_n; i++){
    cam_register_t e = vmc->vm.env;
    cam_value_t v = heap_fst(&vmc->heap, (heap_index)e.value);
    vmc->vm.env = v;
  }
  return 2;
}

int eval_push(vmc_t *vmc, uint8_t *bc_rest) {
  cam_register_t e = vmc->vm.env;
  int i = stack_push(&vmc->vm.stack, e);
  if(i == 0){
    DEBUG_PRINT(("Stack push has failed"));
    return -1;
  }
  (void)bc_rest;
  return 1;
}

int eval_swap(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  int j = stack_push(&vmc->vm.stack, e);
  if(j == 0){
    DEBUG_PRINT(("Stack push has failed"));
    return -1;
  }
  vmc->vm.env = hold_reg;
  return 1;
}

int eval_loadi(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_loadb(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_clear(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_value_t empty_tuple = { .value = 0, .flags = 0 };
  vmc->vm.env = empty_tuple;
  return 1;
}

int eval_cons(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  heap_index hi = heap_allocate(&vmc->heap);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    return -1;
  } else {
    // Assuming we have space for atleast one tuple
    // Do we check this as well?
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->vm.env = env_pointer;
    heap_set(&vmc->heap, hi, hold_reg, e);
    return 1;
  }
}

int eval_cur(vmc_t *vmc, uint8_t *bc_rest) {
  cam_register_t e = vmc->vm.env;
  uint8_t label = bc_rest[0];
  cam_value_t cam_label =
    { .value = (UINT)label, .flags = 0 };
  heap_index hi = heap_allocate(&vmc->heap);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    return -1;
  } else {
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->vm.env = env_pointer;
    heap_set(&vmc->heap, hi, e, cam_label);
    return 2; // read 2 bytes
  }
}

int eval_pack(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_skip(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_stop(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_app(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_return(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_call(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_goto(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_gotofalse(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_switch(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_abs(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_neg(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_not(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_dec(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_add_unsignedi(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value + e.value };
  vmc->vm.env = final_value;
  return 1;
}

int eval_mul_unsignedi(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value * e.value };
  vmc->vm.env = final_value;
  return 1;
}

int eval_min_unsignedi(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value - e.value };
  vmc->vm.env = final_value;
  return 1;
}

int eval_add_signedi(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  INT temp1;
  INT temp2;
  INT temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 + temp2;
  memcpy(&final_value.value, &temp3, sizeof(INT));
  vmc->vm.env = final_value;
  return 1;
}

int eval_mul_signedi(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  INT temp1;
  INT temp2;
  INT temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 * temp2;
  memcpy(&final_value.value, &temp3, sizeof(INT));
  vmc->vm.env = final_value;
  return 1;
}

int eval_min_signedi(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  INT temp1;
  INT temp2;
  INT temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 - temp2;
  memcpy(&final_value.value, &temp3, sizeof(INT));
  vmc->vm.env = final_value;
  return 1;
}


int eval_addf(vmc_t *vmc, uint8_t *bc_rest) {
  (void)bc_rest;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  float temp1;
  float temp2;
  float temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 + temp2;
  memcpy(&final_value.value, &temp3, sizeof(float));
  vmc->vm.env = final_value;
  return 1;
}

int eval_mulf(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_minf(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_gt(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_lt(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_eq(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_ge(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_le(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}
