/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Abhiroop Sarkar             				  */
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

#include <VMC.h>
#include <CAM.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void eval_fst(vmc_t *vmc, INT *pc_idx);
void eval_snd(vmc_t *vmc, INT *pc_idx);
void eval_push(vmc_t *vmc, INT *pc_idx);
void eval_cons(vmc_t *vmc, INT *pc_idx);
void eval_cur(vmc_t *vmc, INT *pc_idx);
void eval_acc(vmc_t *vmc, INT *pc_idx);
void eval_rest(vmc_t *vmc, INT *pc_idx);
void eval_skip(vmc_t *vmc, INT *pc_idx);
void eval_swap(vmc_t *vmc, INT *pc_idx);
void eval_clear(vmc_t *vmc, INT *pc_idx);
void eval_add_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_mul_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_min_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_add_signedi(vmc_t *vmc, INT *pc_idx);
void eval_mul_signedi(vmc_t *vmc, INT *pc_idx);
void eval_min_signedi(vmc_t *vmc, INT *pc_idx);
void eval_addf(vmc_t *vmc, INT *pc_idx);
void eval_mulf(vmc_t *vmc, INT *pc_idx);
void eval_minf(vmc_t *vmc, INT *pc_idx);

bool eval_fst_test(){
  heap_cell_t hc1 = { .fst = 0 }; // DUMMY CELL not used
  heap_cell_t hc2 = { .fst = 5 };
  heap_cell_t heap_array[] = {hc1, hc2};
  heap_flags_t hf1 = { .fst = 0 }; // Unused flag
  heap_flags_t hf2 = { .fst = 0 }; // Unused flag
  heap_flags_t flag_array[] = {hf1, hf2};
  heap_t hp = { .cells = heap_array , .value_flags = flag_array };
  vmc_t vmc = { .heap = hp };
  cam_value_t cv = { .value = 1 };
  vmc.vm.env = cv; // set address at the environment register
  INT pc_idx = 0;
  eval_fst(&vmc, &pc_idx);
  if(vmc.vm.env.value == hc2.fst){
    return true;
  } else {
    return false;
  }
}

bool eval_snd_test(){
  heap_cell_t hc1 = { .fst = 0 }; // DUMMY CELL not used
  heap_cell_t hc2 = { .snd = 5 };
  heap_cell_t heap_array[] = {hc1, hc2};
  heap_flags_t hf1 = { .fst = 0 }; // Unused flag
  heap_flags_t hf2 = { .fst = 0 }; // Unused flag
  heap_flags_t flag_array[] = {hf1, hf2};
  heap_t hp = { .cells = heap_array , .value_flags = flag_array };
  vmc_t vmc = { .heap = hp };
  cam_value_t cv = { .value = 1 };
  vmc.vm.env = cv; // set address at the environment register
  INT pc_idx = 0;
  eval_snd(&vmc, &pc_idx);
  if(vmc.vm.env.value == hc2.snd){
    return true;
  } else {
    return false;
  }
}

bool eval_push_test(){
  cam_value_t cv = { .value = 10, .flags = 0 };
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  VM_t mockvm = { .env = cv, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  cam_register_t dummyreg = { .value = 0 };
  INT pc_idx = 0;
  eval_push(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  int j = stack_pop(&vmc.vm.stack, &dummyreg);
  if (j == 0){
    printf("Stack pop has failed");
    free(m);
    return false;
  }
  free(m);
  if(dummyreg.value == cv.value){
    return true;
  } else {
    return false;
  }
}

bool eval_cons_test(){

  //Value to be held in the environment register
  cam_value_t env_v = { .value = 10, .flags = 0 };

  //Initializing a mock stack
  cam_value_t st_v = { .value = 20, .flags = 0 };
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int s_init = stack_init(&s, m, 256);
  if (s_init == 0){
    printf("Stack initialization has failed");
    return false;
  }
  int y = stack_push(&s, st_v);
  if (y == 0){
    printf("Stack push has failed");
    return false;
  }

  //Initializing a mock heap
  heap_t h = { .size_bytes = 0 };
  uint8_t *hm = malloc(1024);
  int h_init = heap_init(&h, hm, 1024);
  if (h_init == 0){
    printf("Heap initialization has failed");
    return false;
  }

  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm, .heap = h};
  INT pc_idx = 0;
  eval_cons(&vmc, &pc_idx);
  if(pc_idx == -1){
    printf("cons operation has failed\n");
    return false;
  }

  /* heap_show(&vmc.heap, 3); */ //Debugging
  cam_value_t fst = heap_fst(&vmc.heap, (INT)vmc.vm.env.value);
  cam_value_t snd = heap_snd(&vmc.heap, (INT)vmc.vm.env.value);
  free(m); free(hm);
  if(fst.value == st_v.value && snd.value == env_v.value){
    return true;
  } else {
    return false;
  }
}

bool eval_cur_test(){

  //Value to be held in the environment register
  cam_value_t v = { .value = 10 };


  //Initializing a mock heap
  heap_t h = { .size_bytes = 0 };
  uint8_t *hm = malloc(1024);
  int h_init = heap_init(&h, hm, 1024);
  if (h_init == 0){
    printf("Heap initialization has failed");
    return false;
  }

  VM_t mockvm = { .env = v };
  uint8_t code [] = { 10, 0, 1, 3}; // {opcode, label_byte_1, label_byte_2, next opcode}
  vmc_t vmc = { .vm = mockvm, .heap = h, .code_memory = code};
  INT pc_idx = 0;
  eval_cur(&vmc, &pc_idx);
  if(pc_idx == -1){
    printf("cur l operation has failed\n");
    return false;
  }

  //heap_show(&vmc.heap, 3);
  cam_value_t fst = heap_fst(&vmc.heap, (INT)vmc.vm.env.value);
  cam_value_t snd = heap_snd(&vmc.heap, (INT)vmc.vm.env.value);
  free(hm);
  uint16_t merged_label = (code[1] << 8) | code[2];
  if(fst.value == v.value && snd.value == merged_label){
    return true;
  } else {
    return false;
  }
}

bool eval_acc_test(){

  //Initializing a mock heap
  heap_t h = { .size_bytes = 0 };
  uint8_t *hm = malloc(1024);
  int h_init = heap_init(&h, hm, 1024);
  if (h_init == 0){
    printf("Heap initialization has failed");
    free(hm);
    return false;
  }
  heap_index hi = heap_allocate(&h);
  cam_value_t env_pointer =
    { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
  if(hi == HEAP_NULL){
    printf("Heap allocation has failed");
  } else {
    cam_value_t v1 = { .value = 1, .flags = VALUE_PTR_BIT};
    heap_set_fst(&h, hi, v1);
    hi = hi + 1;
    cam_value_t v2 = { .value = 2, .flags = VALUE_PTR_BIT};
    heap_set_fst(&h, hi, v2);
    hi = hi + 1;
    cam_value_t v3 = { .value = 3, .flags = VALUE_PTR_BIT};
    heap_set_fst(&h, hi, v3);
    hi = hi + 1;
    cam_value_t v_actual = { .value = 40 }; // actual value
    heap_set_snd(&h, hi, v_actual);

    // Creates the following heap
    /***************************************************************/
    /* | (1,0) | (32768, 0) | -> | (2,2) | (32768, 32768) |  ->    */
    /* | (3,3) | (32768, 32768) | -> | (0,40) | (0, 0) | ->... */
    /***************************************************************/
  }



  /* heap_show(&h, 5); */
  VM_t mockvm = { .env = env_pointer };
  uint8_t code [] = { 2, 3, 4 }; // {opcode, n, next_opcode}
  vmc_t vmc = { .vm = mockvm, .heap = h, .code_memory = code};


  /* env starts with 0 */
  /* acc 3 */
  /* Step 1 env -> 1 */
  /* Step 2 env -> 2 */
  /* Step 3 env -> 3 */
  /* Step 4 env -> 40 done */
  INT pc_idx = 0;
  eval_acc(&vmc, &pc_idx);
  if(pc_idx != 2){ // read 2 bytes
    printf("cur l operation has failed\n");
    free(hm);
    return false;
  }

  free(hm);
  if(vmc.vm.env.value == 40){
    return true;
  } else {
    return false;
  }
}


bool eval_rest_test(){

  //Initializing a mock heap
  heap_t h = { .size_bytes = 0 };
  uint8_t *hm = malloc(1024);
  int h_init = heap_init(&h, hm, 1024);
  if (h_init == 0){
    printf("Heap initialization has failed");
    free(hm);
    return false;
  }
  heap_index hi = heap_allocate(&h);
  cam_value_t env_pointer =
    { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
  if(hi == HEAP_NULL){
    printf("Heap allocation has failed");
  } else {
    cam_value_t v1 = { .value = 1, .flags = VALUE_PTR_BIT};
    heap_set_fst(&h, hi, v1);
    hi = hi + 1;
    cam_value_t v2 = { .value = 2, .flags = VALUE_PTR_BIT};
    heap_set_fst(&h, hi, v2);
    hi = hi + 1;
    cam_value_t v3 = { .value = 3, .flags = VALUE_PTR_BIT};
    heap_set_fst(&h, hi, v3);
    hi = hi + 1;
    cam_value_t v_actual = { .value = 40 }; // actual value
    heap_set_fst(&h, hi, v_actual);

    // Creates the following heap
    /***************************************************************/
    /* | (1,0) | (32768, 0) | -> | (2,2) | (32768, 32768) |  ->    */
    /* | (3,3) | (32768, 32768) | -> | (40,4) | (0, 32768) | ->... */
    /***************************************************************/
  }



  /* heap_show(&h, 5); */
  VM_t mockvm = { .env = env_pointer };
  uint8_t code [] = { 3, 3, 4 }; //{opcode, n, next_opcode}
  vmc_t vmc = { .vm = mockvm, .heap = h, .code_memory = code};


  /* env starts with 0 */
  /* rest 3 */
  /* Step 1 env -> 1 */
  /* Step 2 env -> 2 */
  /* Step 3 env -> 3 done */
  INT pc_idx = 0;
  eval_rest(&vmc, &pc_idx);
  if(pc_idx != 2){ // read 2 bytes
    printf("cur l operation has failed\n");
    free(hm);
    return false;
  }

  free(hm);
  if(vmc.vm.env.value == 3){
    return true;
  } else {
    return false;
  }
}

bool eval_skip_test(){
  INT pc_idx = 0;
  eval_skip(NULL, &pc_idx);
  if(pc_idx != 1){
    printf("skip operation has failed\n");
    return false;
  } else {
    return true;
  }
}

bool eval_swap_test(){
  cam_value_t env_v = { .value = 10, .flags = 0 };
  cam_value_t st_v  = { .value = 20, .flags = 0 };
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int s_p = stack_push(&s, st_v);
  if(s_p == 0){
    printf("Stack push has failed");
    return false;
  }

  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  cam_register_t dummyreg = { .value = 0 };
  INT pc_idx = 0;
  eval_swap(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  int j = stack_pop(&vmc.vm.stack, &dummyreg);
  if (j == 0){
    printf("Stack pop has failed");
    free(m);
    return false;
  }
  free(m);
  if(dummyreg.value == env_v.value &&
     vmc.vm.env.value == st_v.value){
    return true;
  } else {
    return false;
  }
}

bool eval_clear_test(){
  cam_value_t env_v = { .value = 20, .flags = 0 };

  VM_t mockvm = { .env = env_v };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  eval_clear(&vmc, &pc_idx);
  if(pc_idx != 1){
    printf("clear operation has failed\n");
    return false;
  }
  if(vmc.vm.env.value == 0 && vmc.vm.env.flags == 0) {
    return true;
  } else {
    return false;
  }
}

bool eval_add_unsignedi_test(){
  cam_value_t env_v = { .value = 15, .flags = 0 };
  cam_value_t st_v  = { .value = 10, .flags = 0 };
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int s_p = stack_push(&s, st_v);
  if(s_p == 0){
    printf("Stack push has failed");
    return false;
  }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  eval_add_unsignedi(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  free(m);
  if(vmc.vm.env.value ==  st_v.value + env_v.value){
    return true;
  } else {
    return false;
  }
}

bool eval_mul_unsignedi_test(){
  cam_value_t env_v = { .value = 15, .flags = 0 };
  cam_value_t st_v  = { .value = 10, .flags = 0 };
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int s_p = stack_push(&s, st_v);
  if(s_p == 0){
    printf("Stack push has failed");
    return false;
  }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  eval_mul_unsignedi(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  free(m);
  if(vmc.vm.env.value ==  st_v.value * env_v.value){
    return true;
  } else {
    return false;
  }
}

bool eval_min_unsignedi_test(){
  cam_value_t env_v = { .value = 15, .flags = 0 };
  cam_value_t st_v  = { .value = 10, .flags = 0 };
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int s_p = stack_push(&s, st_v);
  if(s_p == 0){
    printf("Stack push has failed");
    return false;
  }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  eval_min_unsignedi(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  free(m);
  if(vmc.vm.env.value == st_v.value - env_v.value){
    return true;
  } else {
    return false;
  }
}


bool eval_add_signedi_test(){
  cam_value_t env_v = { .flags = 0 };
  cam_value_t st_v  = { .flags = 0 };
  INT e_val = -15;
  INT s_val = -10;
  memcpy(&env_v.value, &e_val, sizeof(INT));
  memcpy(&st_v.value, &s_val, sizeof(INT));
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int s_p = stack_push(&s, st_v);
  if(s_p == 0){
    printf("Stack push has failed");
    return false;
  }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  eval_add_signedi(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  free(m);
  INT result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val + e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_mul_signedi_test(){
  cam_value_t env_v = { .flags = 0 };
  cam_value_t st_v  = { .flags = 0 };
  INT e_val = -15;
  INT s_val = -10;
  memcpy(&env_v.value, &e_val, sizeof(INT));
  memcpy(&st_v.value, &s_val, sizeof(INT));
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int s_p = stack_push(&s, st_v);
  if(s_p == 0){
    printf("Stack push has failed");
    return false;
  }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  eval_mul_signedi(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  free(m);
  INT result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val * e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_min_signedi_test(){
  cam_value_t env_v = { .flags = 0 };
  cam_value_t st_v  = { .flags = 0 };
  INT e_val = -15;
  INT s_val = -10;
  memcpy(&env_v.value, &e_val, sizeof(INT));
  memcpy(&st_v.value, &s_val, sizeof(INT));
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int s_p = stack_push(&s, st_v);
  if(s_p == 0){
    printf("Stack push has failed");
    return false;
  }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  eval_min_signedi(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  free(m);
  INT result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val - e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_addf_test(){
  cam_value_t env_v = { .flags = 0 };
  cam_value_t st_v  = { .flags = 0 };
  float e_val = 4.389;
  float s_val = 2.456;
  memcpy(&env_v.value, &e_val, sizeof(float));
  memcpy(&st_v.value, &s_val, sizeof(float));
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int s_p = stack_push(&s, st_v);
  if(s_p == 0){
    printf("Stack push has failed");
    return false;
  }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  eval_addf(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  free(m);
  float result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val + e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_mulf_test(){
  cam_value_t env_v = { .flags = 0 };
  cam_value_t st_v  = { .flags = 0 };
  float e_val = 4.389;
  float s_val = 2.456;
  memcpy(&env_v.value, &e_val, sizeof(float));
  memcpy(&st_v.value, &s_val, sizeof(float));
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int s_p = stack_push(&s, st_v);
  if(s_p == 0){
    printf("Stack push has failed");
    return false;
  }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  eval_mulf(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  free(m);
  float result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val * e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_minf_test(){
  cam_value_t env_v = { .flags = 0 };
  cam_value_t st_v  = { .flags = 0 };
  float e_val = 4.389;
  float s_val = 2.456;
  memcpy(&env_v.value, &e_val, sizeof(float));
  memcpy(&st_v.value, &s_val, sizeof(float));
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int s_p = stack_push(&s, st_v);
  if(s_p == 0){
    printf("Stack push has failed");
    return false;
  }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  eval_minf(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("push operation has failed");
    free(m);
    return false;
  }
  free(m);
  float result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val - e_val){
    return true;
  } else {
    return false;
  }
}


void test_stat(char *s, int *tot, bool t){
  if (t) {
    (*tot)++;
    printf("%s unit test passed\n", s);
  } else {
    printf("%s unit test failed\n", s);
  }
}

int main(int argc, char **argv) {
  (void)argc;
  (void)argv;
  int total = 0;


  bool t1 = eval_fst_test();
  test_stat("eval_fst", &total, t1);
  bool t2 = eval_snd_test();
  test_stat("eval_snd", &total, t2);
  bool t3 = eval_push_test();
  test_stat("eval_push", &total, t3);
  bool t4 = eval_cons_test();
  test_stat("eval_cons", &total, t4);
  bool t5 = eval_cur_test();
  test_stat("eval_cur", &total, t5);
  bool t6 = eval_acc_test();
  test_stat("eval_acc", &total, t6);
  bool t7 = eval_rest_test();
  test_stat("eval_rest", &total, t7);
  bool t8 = eval_skip_test();
  test_stat("eval_skip", &total, t8);
  bool t9 = eval_swap_test();
  test_stat("eval_swap", &total, t9);
  bool t10 = eval_clear_test();
  test_stat("eval_clear", &total, t10);
  bool t11 = eval_add_unsignedi_test();
  test_stat("eval_add_unsignedi", &total, t11);
  bool t12 = eval_mul_unsignedi_test();
  test_stat("eval_mul_unsignedi", &total, t12);
  bool t13 = eval_min_unsignedi_test();
  test_stat("eval_min_unsignedi", &total, t13);
  bool t14 = eval_add_signedi_test();
  test_stat("eval_add_signedi", &total, t14);
  bool t15 = eval_mul_signedi_test();
  test_stat("eval_mul_signedi", &total, t15);
  bool t16 = eval_min_signedi_test();
  test_stat("eval_min_signedi", &total, t16);
  bool t17 = eval_addf_test();
  test_stat("eval_addf", &total, t17);
  bool t18 = eval_mulf_test();
  test_stat("eval_mulf", &total, t18);
  bool t19 = eval_minf_test();
  test_stat("eval_minf", &total, t19);

  printf("Passed total : %d/%d tests\n", total, 19);
  return 1;
}
