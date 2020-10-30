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
  (*evaluators[0])(&vmc, &pc_idx);
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
  (*evaluators[1])(&vmc, &pc_idx);
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
  (*evaluators[4])(&vmc, &pc_idx);
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
    free(m);
    return false;
  }
  int y = stack_push(&s, st_v);
  if (y == 0){
    printf("Stack push has failed");
    free(m);
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
  (*evaluators[9])(&vmc, &pc_idx);
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
  (*evaluators[10])(&vmc, &pc_idx);
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
  (*evaluators[2])(&vmc, &pc_idx);
  if(pc_idx != 2){ // read 2 bytes
    printf("acc n operation has failed\n");
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
  (*evaluators[3])(&vmc, &pc_idx);
  if(pc_idx != 2){ // read 2 bytes
    printf("rest n operation has failed\n");
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
  (*evaluators[12])(NULL, &pc_idx);
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
  (*evaluators[5])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("swap operation has failed");
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
  (*evaluators[8])(&vmc, &pc_idx);
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
  (*evaluators[35])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("add_unsigned_i operation has failed");
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
  (*evaluators[36])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("mul_unsigned_i operation has failed");
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
  (*evaluators[37])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("min_unsigned_i operation has failed");
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
  (*evaluators[24])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("add_signed_i operation has failed");
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
  (*evaluators[25])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("mul_signed_i operation has failed");
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
  (*evaluators[26])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("min_signed_i operation has failed");
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
  (*evaluators[27])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("addf operation has failed");
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
  (*evaluators[28])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("mulf operation has failed");
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
  (*evaluators[29])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("minf operation has failed");
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

bool eval_call_test(){

  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  uint8_t code [] = { 24, 16, 0, 0, 20 }; //{addi, call, x00, x00, absinst}
  VM_t mockvm = { .stack = s };
  vmc_t vmc = { .vm = mockvm, .code_memory = code};

  INT pc_idx = 1;
  (*evaluators[16])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("call operation has failed");
    free(m);
    return false;
  }
  cam_register_t dummyreg = { .value = 0 };
  int j = stack_pop(&vmc.vm.stack, &dummyreg);
  if (j == 0){
    printf("Stack pop has failed");
    free(m);
    return false;
  }
  free(m);
  if(pc_idx == 0 && dummyreg.value == 4){
    return true;
  } else {
    return false;
  }
}

bool eval_goto_test(){

  uint8_t code [] = { 17, 0, 5 }; //{goto, x00, x05}
  vmc_t vmc = { .code_memory = code };

  INT pc_idx = 0;
  (*evaluators[17])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("goto operation has failed");
    return false;
  }
  if(pc_idx == 5){
    return true;
  } else {
    return false;
  }
}

bool eval_return_test(){

  //Initializing a mock stack
  cam_value_t st_v = { .value = 20, .flags = 0 };
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int y = stack_push(&s, st_v);
  if (y == 0){
    printf("Stack push has failed");
    return false;
  }
  VM_t mockvm = { .stack = s };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  (*evaluators[15])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("return operation has failed");
    free(m);
    return false;
  }
  free(m);
  if(pc_idx == 20){
    return true;
  } else {
    return false;
  }
}

bool eval_app_test(){

  //Initializing a mock stack
  cam_value_t st_v = { .value = 3, .flags = 0 };
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int y = stack_push(&s, st_v);
  if (y == 0){
    printf("Stack push has failed");
    free(m);
    return false;
  }


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
  if(hi == HEAP_NULL){
    printf("Heap allocation has failed");
    free(hm);
    return false;
  }
  cam_value_t vempty  = { .value = 0, .flags = 0};
  cam_value_t v_label = { .value = 2, .flags = 0};
  heap_set_fst(&h, hi, vempty);
  heap_set_snd(&h, hi, v_label);
  cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };


  uint8_t code [] = { 14, 13, 2, 0 }; //{app, stop, acc, x00 }
  VM_t mockvm = { .stack = s, .env = env_pointer };
  vmc_t vmc = { .vm = mockvm, .heap = h, .code_memory = code};
  INT pc_idx = 0;

  // Mock Machine state before eval_app
  /*****************************************/
  /*  env   =  (Ptr 0)                     */
  /*  stack =  3 -> Empty                  */
  /*  heap  =  | (VEmpty, 2) | -> HEAP_END */
  /*  code  =  app, stop, acc, x00         */
  /*           ^                           */
  /*           |                           */
  /*        PC = 0                         */
  /*****************************************/

  (*evaluators[14])(&vmc, &pc_idx);

  // Machine state post eval_app
  /**********************************************************/
  /*  env   =  (Ptr 1)                                      */
  /*  stack =  (JAdd 1) -> Empty                            */
  /*  heap  =  | (VEmpty, 2) | -> |(VEmpty, 3)| -> HEAP_END */
  /*  code  =  app, stop, acc, x00                          */
  /*                      ^                                 */
  /*                      |                                 */
  /*                   PC = 2                               */
  /**********************************************************/

  if (pc_idx == -1){
    printf("app operation has failed");
    free(m); free(hm);
    return false;
  }
  cam_register_t dummyreg = { .value = 0 };
  int j = stack_pop(&vmc.vm.stack, &dummyreg);
  if (j == 0){
    printf("Stack pop has failed");
    free(m); free(hm);
    return false;
  }
  cam_value_t fst = heap_fst(&vmc.heap, (INT)vmc.vm.env.value);
  cam_value_t snd = heap_snd(&vmc.heap, (INT)vmc.vm.env.value);

  free(m);
  free(hm);
  if(pc_idx == (INT)v_label.value &&  // Test PC
     (INT)dummyreg.value == 1 && // initial pc_idx + 1; Test stack top
     fst.value == vempty.value && snd.value == st_v.value){ // Test heap
    return true;
  } else {
    return false;
  }
}

bool eval_gotofalse_t_test(){
  cam_value_t env_v = { .value = 1, .flags = 0 }; // TRUE
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  cam_value_t st_v = { .value = 20, .flags = 0 };
  int y = stack_push(&s, st_v);
  if (y == 0){
    printf("Stack push has failed");
    free(m);
    return false;
  }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc   = { .vm = mockvm };
  INT pc_idx = 0;
  (*evaluators[18])(&vmc, &pc_idx);
  if(pc_idx == -1){
    printf("gotofalse operation has failed\n");
    free(m);
    return false;
  }
  free(m);
  if(vmc.vm.env.value == st_v.value &&
     pc_idx == 3){
    return true;
  } else {
    return false;
  }
}
bool eval_gotofalse_f_test(){
  cam_value_t env_v = { .value = 0, .flags = 0 }; // FALSE
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  cam_value_t st_v = { .value = 20, .flags = 0 };
  int y = stack_push(&s, st_v);
  if (y == 0){
    printf("Stack push has failed");
    free(m);
    return false;
  }
  uint8_t code [] = { 18, 0, 5 }; //{gotofalse, x00, x05 }
  VM_t mockvm = { .env = env_v, .stack = s };
  vmc_t vmc   = { .vm = mockvm, .code_memory = code };
  INT pc_idx = 0;
  (*evaluators[18])(&vmc, &pc_idx);
  if(pc_idx == -1){
    printf("gotofalse operation has failed\n");
    free(m);
    return false;
  }
  free(m);
  uint16_t merged_label = (code[1] << 8) | code[2];
  if(vmc.vm.env.value == st_v.value &&
     pc_idx == (INT)merged_label){
    return true;
  } else {
    return false;
  }
}

bool eval_loadi_test(){
  uint8_t code [] = { 254, 237, 202, 254, 255, 0, 1, // magic code, pool size
                      255, 255, 255, 236, 6, 0, 0 }; // {-20, loadi, x00, x00}
  vmc_t vmc   = { .code_memory = code };
  INT pc_idx = 11;
  (*evaluators[6])(&vmc, &pc_idx);
  // No Failure cases
  if((INT)vmc.vm.env.value == -20 && pc_idx == 14){ // old pc_idx + 3
    return true;
  } else {
    return false;
  }
}

bool eval_loadb_test(){
  uint8_t code [] = { 7, 1 }; // {loadb, x01 } TRUE
  vmc_t vmc   = { .code_memory = code };
  INT pc_idx = 0;
  (*evaluators[7])(&vmc, &pc_idx);
  // No Failure cases
  if(vmc.vm.env.value == 1 && pc_idx == 2){ // old pc_idx + 2
    return true;
  } else {
    return false;
  }
}

bool eval_abs_test(){

  cam_value_t cv = { .value = -10, .flags = 0 };
  VM_t mockvm = { .env = cv };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  (*evaluators[20])(&vmc, &pc_idx);
  // No Failure cases
  if((INT)vmc.vm.env.value == 10 && pc_idx == 1){
    return true;
  } else {
    return false;
  }
}

bool eval_neg_test(){

  cam_value_t cv = { .value = 10, .flags = 0 };
  VM_t mockvm = { .env = cv };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  (*evaluators[21])(&vmc, &pc_idx);
  // No Failure cases; possible underflow
  if((INT)vmc.vm.env.value == -10 && pc_idx == 1){
    return true;
  } else {
    return false;
  }
}

bool eval_not_test(){

  cam_value_t cv = { .value = 0, .flags = 0 }; // false
  VM_t mockvm = { .env = cv };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  (*evaluators[22])(&vmc, &pc_idx);
  // No Failure cases
  UINT t = vmc.vm.env.value;

  // Now the environment holds true
  (*evaluators[22])(&vmc, &pc_idx);
  UINT f = vmc.vm.env.value;

  if(t == 1 && f == 0){
    return true;
  } else {
    return false;
  }
}

bool eval_dec_test(){

  cam_value_t cv = { .value = (UINT)-5, .flags = 0 };
  VM_t mockvm = { .env = cv };
  vmc_t vmc = { .vm = mockvm };

  INT pc_idx = 0;
  (*evaluators[23])(&vmc, &pc_idx);
  // No Failure cases; possible underflow
  if((INT)vmc.vm.env.value == -6 && pc_idx == 1){
    return true;
  } else {
    return false;
  }
}

bool eval_gt_unsignedi_test(){
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
  (*evaluators[38])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("gt_unsigned_i operation has failed");
    free(m);
    return false;
  }
  free(m);
  if(vmc.vm.env.value ==  st_v.value > env_v.value){
    return true;
  } else {
    return false;
  }
}

bool eval_lt_unsignedi_test(){
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
  (*evaluators[39])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("lt_unsigned_i operation has failed");
    free(m);
    return false;
  }
  free(m);
  if(vmc.vm.env.value ==  st_v.value < env_v.value){
    return true;
  } else {
    return false;
  }
}

bool eval_ge_unsignedi_test(){
  cam_value_t env_v = { .value = 10, .flags = 0 };
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
  (*evaluators[41])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("ge_unsigned_i operation has failed");
    free(m);
    return false;
  }
  free(m);
  if(vmc.vm.env.value ==  st_v.value >= env_v.value){
    return true;
  } else {
    return false;
  }
}

bool eval_le_unsignedi_test(){
  cam_value_t env_v = { .value = 5, .flags = 0 };
  cam_value_t st_v  = { .value = 5, .flags = 0 };
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
  (*evaluators[42])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("le_unsigned_i operation has failed");
    free(m);
    return false;
  }
  free(m);
  if(vmc.vm.env.value ==  st_v.value <= env_v.value){
    return true;
  } else {
    return false;
  }
}

bool eval_gt_signedi_test(){
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
  (*evaluators[30])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("gt_signed_i operation has failed");
    free(m);
    return false;
  }
  free(m);
  INT result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val > e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_lt_signedi_test(){
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
  (*evaluators[31])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("lt_signed_i operation has failed");
    free(m);
    return false;
  }
  free(m);
  INT result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val < e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_ge_signedi_test(){
  cam_value_t env_v = { .flags = 0 };
  cam_value_t st_v  = { .flags = 0 };
  INT e_val = -15;
  INT s_val = -15;
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
  (*evaluators[33])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("ge_signed_i operation has failed");
    free(m);
    return false;
  }
  free(m);
  INT result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val >= e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_le_signedi_test(){
  cam_value_t env_v = { .flags = 0 };
  cam_value_t st_v  = { .flags = 0 };
  INT e_val = -15;
  INT s_val = -15;
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
  (*evaluators[34])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("le_signed_i operation has failed");
    free(m);
    return false;
  }
  free(m);
  INT result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val <= e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_gtf_test(){
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
  (*evaluators[43])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("gtf operation has failed");
    free(m);
    return false;
  }
  free(m);
  float result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val > e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_ltf_test(){
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
  (*evaluators[44])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("ltf operation has failed");
    free(m);
    return false;
  }
  free(m);
  float result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val < e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_gef_test(){
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
  (*evaluators[46])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("gef operation has failed");
    free(m);
    return false;
  }
  free(m);
  float result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val >= e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_lef_test(){
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
  (*evaluators[47])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("lef operation has failed");
    free(m);
    return false;
  }
  free(m);
  float result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == s_val <= e_val){
    return true;
  } else {
    return false;
  }
}

bool eval_eq_unsignedi_test(){
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
  (*evaluators[40])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("eq_unsigned_i operation has failed");
    free(m);
    return false;
  }
  free(m);
  if(vmc.vm.env.value ==  (st_v.value == env_v.value)){
    return true;
  } else {
    return false;
  }
}

bool eval_eq_signedi_test(){
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
  (*evaluators[32])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("eq_signed_i operation has failed");
    free(m);
    return false;
  }
  free(m);
  INT result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == (s_val == e_val)){
    return true;
  } else {
    return false;
  }
}

bool eval_eqf_test(){
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
  (*evaluators[45])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("eqf operation has failed");
    free(m);
    return false;
  }
  free(m);
  float result;
  memcpy(&result, &vmc.vm.env.value, sizeof(UINT));
  if(result == (s_val == e_val)){
    return true;
  } else {
    return false;
  }
}

bool eval_eq_bool_test(){
  cam_value_t env_v = { .value = 1, .flags = 0 }; // true
  cam_value_t st_v  = { .value = 1, .flags = 0 }; // true
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
  (*evaluators[48])(&vmc, &pc_idx);
  if (pc_idx == -1){
    printf("eq_bool operation has failed");
    free(m);
    return false;
  }
  free(m);
  if(vmc.vm.env.value ==  (st_v.value == env_v.value)){
    return true;
  } else {
    return false;
  }
}

bool eval_pack_test(){

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
  uint8_t code [] = { 11, 0, 1, 3}; // {opcode, label_byte_1, label_byte_2, next opcode}
  vmc_t vmc = { .vm = mockvm, .heap = h, .code_memory = code};
  INT pc_idx = 0;
  (*evaluators[11])(&vmc, &pc_idx);
  if(pc_idx == -1){
    printf("pack t operation has failed\n");
    return false;
  }

  //heap_show(&vmc.heap, 3);
  cam_value_t fst = heap_fst(&vmc.heap, (INT)vmc.vm.env.value);
  cam_value_t snd = heap_snd(&vmc.heap, (INT)vmc.vm.env.value);
  free(hm);
  uint16_t merged_tag = (code[1] << 8) | code[2];
  if(fst.value == merged_tag && snd.value == v.value){
    return true;
  } else {
    return false;
  }
}

bool eval_switch_test(){

  //Initializing a mock stack
  cam_value_t st_v = { .value = 3, .flags = 0 };
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int w = stack_init(&s, m, 256);
  if (w == 0){
    printf("Stack initialization has failed");
    free(m);
    return false;
  }
  int y = stack_push(&s, st_v);
  if (y == 0){
    printf("Stack push has failed");
    free(m);
    return false;
  }


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
  if(hi == HEAP_NULL){
    printf("Heap allocation has failed");
    free(hm);
    return false;
  }
  cam_value_t vempty  = { .value = 0, .flags = 0};
  cam_value_t c_tag = { .value = 2, .flags = 0}; // imaginary tag with value 2
  heap_set_fst(&h, hi, c_tag);
  heap_set_snd(&h, hi, vempty);
  cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };

  /*
  {switch, x02,           // opcode, size
   x00, x01, x00, x0f,    // tag1(2 bytes), label1(2 bytes)
   x00, x02, x00, x0b,    // tag2(2 bytes), label2(2 bytes)
   stop, skip }           // next opcodes
  */
  uint8_t code [] = { 19, 2, 0, 1, 0, 15, 0, 2, 0, 11, 13, 12 };
  VM_t mockvm = { .stack = s, .env = env_pointer };
  vmc_t vmc = { .vm = mockvm, .heap = h, .code_memory = code};
  INT pc_idx = 0;

  // Mock Machine state before eval_switch
  /*****************************************************************************/
  /*  env   =  (Ptr 0)                                                         */
  /*  stack =  3 -> Empty                                                      */
  /*  heap  =  | (2, VEmpty) | -> HEAP_END                                     */
  /*  code  =  switch, x02, x00, x01, x00, x0f, x00, x02, x00, x0b, stop, skip */
  /*           ^                                                               */
  /*           |                                                               */
  /*        PC = 0                                                             */
  /*****************************************************************************/

  (*evaluators[19])(&vmc, &pc_idx);

  // Machine state post eval_app
  /******************************************************************************/
  /*  env   =  (Ptr 1)                                                          */
  /*  stack =  (JAdd 10) -> Empty                                               */
  /*  heap  =  | (2, VEmpty) | -> |(3, VEmpty)| -> HEAP_END                     */
  /*  code  =  switch, x02, x00, x01, x00, x0f, x00, x02, x00, x0b, stop, skip  */
  /*                                                                      ^     */
  /*                                                                      |     */
  /*                                                                    PC = 11 */
  /******************************************************************************/

  if (pc_idx == -1){
    printf("switch operation has failed");
    free(m); free(hm);
    return false;
  }
  cam_register_t dummyreg = { .value = 0 };
  int j = stack_pop(&vmc.vm.stack, &dummyreg);
  if (j == 0){
    printf("Stack pop has failed");
    free(m); free(hm);
    return false;
  }
  cam_value_t fst = heap_fst(&vmc.heap, (INT)vmc.vm.env.value);
  cam_value_t snd = heap_snd(&vmc.heap, (INT)vmc.vm.env.value);

  free(m);
  free(hm);
  if(pc_idx == 11 &&  // Test PC // label associated with tag 2
     (INT)dummyreg.value == 10 && // initial pc_idx + 1 + 1 + 2 * 4; Test stack top
     fst.value == st_v.value && snd.value == vempty.value){ // Test heap
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
  bool t20 = eval_call_test();
  test_stat("eval_call", &total, t20);
  bool t21 = eval_goto_test();
  test_stat("eval_goto", &total, t21);
  bool t22 = eval_return_test();
  test_stat("eval_return", &total, t22);
  bool t23 = eval_app_test();
  test_stat("eval_app", &total, t23);
  bool t24 = eval_gotofalse_t_test();
  test_stat("eval_gotofalse_t", &total, t24);
  bool t25 = eval_gotofalse_f_test();
  test_stat("eval_gotofalse_f", &total, t25);
  bool t26 = eval_loadi_test();
  test_stat("eval_loadi", &total, t26);
  bool t27 = eval_loadb_test();
  test_stat("eval_loadb", &total, t27);
  bool t28 = eval_abs_test();
  test_stat("eval_abs", &total, t28);
  bool t29 = eval_neg_test();
  test_stat("eval_neg", &total, t29);
  bool t30 = eval_not_test();
  test_stat("eval_not", &total, t30);
  bool t31 = eval_dec_test();
  test_stat("eval_dec", &total, t31);
  bool t32 = eval_gt_unsignedi_test();
  test_stat("eval_gt_unsignedi", &total, t32);
  bool t33 = eval_lt_unsignedi_test();
  test_stat("eval_lt_unsignedi", &total, t33);
  bool t34 = eval_ge_unsignedi_test();
  test_stat("eval_ge_unsignedi", &total, t34);
  bool t35 = eval_le_unsignedi_test();
  test_stat("eval_le_unsignedi", &total, t35);
  bool t36 = eval_gt_signedi_test();
  test_stat("eval_gt_signedi", &total, t36);
  bool t37 = eval_lt_signedi_test();
  test_stat("eval_lt_signedi", &total, t37);
  bool t38 = eval_ge_signedi_test();
  test_stat("eval_ge_signedi", &total, t38);
  bool t39 = eval_le_signedi_test();
  test_stat("eval_lt_signedi", &total, t39);
  bool t40 = eval_gtf_test();
  test_stat("eval_gtf", &total, t40);
  bool t41 = eval_ltf_test();
  test_stat("eval_ltf", &total, t41);
  bool t42 = eval_gef_test();
  test_stat("eval_gef", &total, t42);
  bool t43 = eval_lef_test();
  test_stat("eval_lef", &total, t43);
  bool t44 = eval_eq_unsignedi_test();
  test_stat("eval_eq_unsignedi", &total, t44);
  bool t45 = eval_eq_signedi_test();
  test_stat("eval_eq_signedi", &total, t45);
  bool t46 = eval_eqf_test();
  test_stat("eval_eqf", &total, t46);
  bool t47 = eval_eq_bool_test();
  test_stat("eval_eq_bool", &total, t47);
  bool t48 = eval_pack_test();
  test_stat("eval_pack", &total, t48);
  bool t49 = eval_switch_test();
  test_stat("eval_switch", &total, t49);

  printf("Passed total : %d/%d tests\n", total, 49);
  return 1;
}
