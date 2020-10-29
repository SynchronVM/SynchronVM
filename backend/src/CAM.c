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
#include <stdlib.h>
#include <string.h>

/* Each eval function is called with the vmc state and the
 * current index of the program counter (pointed at the opcode).
 * The eval function internally increments the pc_idx. The
 * caller simply checks if pc_idx ever returns a negative value,
 * which contains semantic error info.
 */

/*Jump convention:
 * When making a jump, calculate the jump address + arguments
 * and store that on the stack. A return simply pops off that
 * address which is the address of the next opcode and jumps
 */

void eval_fst(vmc_t *vmc, INT *pc_idx);
void eval_snd(vmc_t *vmc, INT *pc_idx);
void eval_acc(vmc_t *vmc, INT *pc_idx);
void eval_rest(vmc_t *vmc, INT *pc_idx);
void eval_push(vmc_t *vmc, INT *pc_idx);
void eval_swap(vmc_t *vmc, INT *pc_idx);
void eval_loadi(vmc_t *vmc, INT *pc_idx);
void eval_loadb(vmc_t *vmc, INT *pc_idx);
void eval_clear(vmc_t *vmc, INT *pc_idx);
void eval_cons(vmc_t *vmc, INT *pc_idx);
void eval_cur(vmc_t *vmc, INT *pc_idx);
void eval_pack(vmc_t *vmc, INT *pc_idx);
void eval_skip(vmc_t *vmc, INT *pc_idx);
void eval_stop(vmc_t *vmc, INT *pc_idx);
void eval_app(vmc_t *vmc, INT *pc_idx);
void eval_return(vmc_t *vmc, INT *pc_idx);
void eval_call(vmc_t *vmc, INT *pc_idx);
void eval_goto(vmc_t *vmc, INT *pc_idx);
void eval_gotofalse(vmc_t *vmc, INT *pc_idx);
void eval_switch(vmc_t *vmc, INT *pc_idx);
void eval_abs(vmc_t *vmc, INT *pc_idx);
void eval_neg(vmc_t *vmc, INT *pc_idx);
void eval_not(vmc_t *vmc, INT *pc_idx);
void eval_dec(vmc_t *vmc, INT *pc_idx);
void eval_add_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_mul_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_min_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_add_signedi(vmc_t *vmc, INT *pc_idx);
void eval_mul_signedi(vmc_t *vmc, INT *pc_idx);
void eval_min_signedi(vmc_t *vmc, INT *pc_idx);
void eval_addf(vmc_t *vmc, INT *pc_idx);
void eval_mulf(vmc_t *vmc, INT *pc_idx);
void eval_minf(vmc_t *vmc, INT *pc_idx);
void eval_gt_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_lt_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_ge_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_le_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_gt_signedi(vmc_t *vmc, INT *pc_idx);
void eval_lt_signedi(vmc_t *vmc, INT *pc_idx);
void eval_ge_signedi(vmc_t *vmc, INT *pc_idx);
void eval_le_signedi(vmc_t *vmc, INT *pc_idx);
void eval_gtf(vmc_t *vmc, INT *pc_idx);
void eval_ltf(vmc_t *vmc, INT *pc_idx);
void eval_gef(vmc_t *vmc, INT *pc_idx);
void eval_lef(vmc_t *vmc, INT *pc_idx);
void eval_eq_unsignedi(vmc_t *vmc, INT *pc_idx);
void eval_eq_signedi(vmc_t *vmc, INT *pc_idx);
void eval_eqf(vmc_t *vmc, INT *pc_idx);
void eval_eq_bool(vmc_t *vmc, INT *pc_idx);

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
    eval_add_signedi,
    eval_mul_signedi,
    eval_min_signedi,
    eval_addf,
    eval_mulf,
    eval_minf,
    eval_gt_signedi,
    eval_lt_signedi,
    eval_eq_signedi,
    eval_ge_signedi,
    eval_le_signedi,
    eval_add_unsignedi,
    eval_mul_unsignedi,
    eval_min_unsignedi,
    eval_gt_unsignedi,
    eval_lt_unsignedi,
    eval_ge_unsignedi,
    eval_le_unsignedi,
    eval_gtf,
    eval_ltf,
    eval_gef,
    eval_lef,
    eval_eq_unsignedi,
    eval_eqf,
    eval_eq_bool };

uint16_t get_label(vmc_t *vmc, INT *pc_idx){
  INT lab_idx1 = (*pc_idx) + 1;
  INT lab_idx2 = (*pc_idx) + 2;
  uint16_t label =
    (vmc->code_memory[lab_idx1] << 8) | vmc->code_memory[lab_idx2]; // merge 2 bytes
  return label;
}

uint16_t get_tag(vmc_t *vmc, INT *pc_idx){
  INT tag_idx1 = (*pc_idx) + 1;
  INT tag_idx2 = (*pc_idx) + 2;
  uint16_t tag =
    (vmc->code_memory[tag_idx1] << 8) | vmc->code_memory[tag_idx2]; // merge 2 bytes
  return tag;
}

void eval_fst(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_value_t v = heap_fst(&vmc->heap, (heap_index)e.value);
  vmc->vm.env = v;
}

void eval_snd(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_value_t v = heap_snd(&vmc->heap, (heap_index)e.value);
  vmc->vm.env = v;
}


void eval_acc(vmc_t *vmc, INT *pc_idx) {
  INT n_idx = (*pc_idx) + 1;
  uint8_t acc_n = vmc->code_memory[n_idx];
  for(unsigned int i = 0; i < acc_n; i++){
    cam_register_t e = vmc->vm.env;
    cam_value_t v = heap_fst(&vmc->heap, (heap_index)e.value);
    vmc->vm.env = v;
  }
  cam_value_t v = heap_snd(&vmc->heap, (heap_index)vmc->vm.env.value);
  vmc->vm.env = v;
  *pc_idx = (*pc_idx) + 2;
}

void eval_rest(vmc_t *vmc, INT *pc_idx)  {
  INT n_idx = (*pc_idx) + 1;
  uint8_t acc_n = vmc->code_memory[n_idx];
  for(unsigned int i = 0; i < acc_n; i++){
    cam_register_t e = vmc->vm.env;
    cam_value_t v = heap_fst(&vmc->heap, (heap_index)e.value);
    vmc->vm.env = v;
  }
  *pc_idx = (*pc_idx) + 2;
}

void eval_push(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->vm.env;
  int i = stack_push(&vmc->vm.stack, e);
  if(i == 0){
    DEBUG_PRINT(("Stack push has failed"));
    *pc_idx = -1;
    return;
  }
  (*pc_idx)++;
}

void eval_swap(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  int j = stack_push(&vmc->vm.stack, e);
  if(j == 0){
    DEBUG_PRINT(("Stack push has failed"));
    *pc_idx = -1;
    return;
  }
  vmc->vm.env = hold_reg;
  (*pc_idx)++;
}

void eval_loadi(vmc_t *vmc, INT *pc_idx) {
  INT int_idx1 = (*pc_idx) + 1;
  INT int_idx2 = (*pc_idx) + 2;
  uint16_t int_idx =
    (vmc->code_memory[int_idx1] << 8) | vmc->code_memory[int_idx2]; // merge 2 bytes
  INT int_pool_offset = 7; //TODO: Should we verify the int pool size here?
  INT i_idx = int_pool_offset + int_idx;
  uint8_t byte0 = vmc->code_memory[i_idx];
  uint8_t byte1 = vmc->code_memory[i_idx + 1];
  uint8_t byte2 = vmc->code_memory[i_idx + 2];
  uint8_t byte3 = vmc->code_memory[i_idx + 3];
  INT i = (byte0 << 24) | (byte1 << 16) | (byte2 << 8) | byte3;
  cam_value_t v = { .value = (UINT)i, .flags = 0};
  vmc->vm.env = v;
  *pc_idx = (*pc_idx) + 3;
}

void eval_loadb(vmc_t *vmc, INT *pc_idx) {
  INT bool_idx = (*pc_idx) + 1;
  uint8_t bool_val = vmc->code_memory[bool_idx];
  cam_value_t v = { .value = (UINT)bool_val, .flags = 0};
  vmc->vm.env = v;
  *pc_idx = (*pc_idx) + 2;
}

void eval_clear(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_value_t empty_tuple = { .value = 0, .flags = 0 };
  vmc->vm.env = empty_tuple;
}

void eval_cons(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  heap_index hi = heap_allocate(&vmc->heap);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  } else {
    // Assuming we have space for atleast one tuple
    // Do we check this as well?
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->vm.env = env_pointer;
    heap_set(&vmc->heap, hi, hold_reg, e);
  }
}

void eval_cur(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->vm.env;
  uint16_t label = get_label(vmc, pc_idx);
  cam_value_t cam_label =
    { .value = (UINT)label, .flags = 0 };
  heap_index hi = heap_allocate(&vmc->heap);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  } else {
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->vm.env = env_pointer;
    heap_set(&vmc->heap, hi, e, cam_label);
    *pc_idx = (*pc_idx) + 3;
  }
}

void eval_pack(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->vm.env;
  uint16_t tag = get_tag(vmc, pc_idx);
  cam_value_t cam_tag =
    { .value = (UINT)tag, .flags = 0 };
  heap_index hi = heap_allocate(&vmc->heap);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  } else {
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->vm.env = env_pointer;
    heap_set(&vmc->heap, hi, cam_tag, e);
    *pc_idx = (*pc_idx) + 3;
  }
}

void eval_skip(vmc_t *vmc, INT *pc_idx) {
  (void)vmc;
  (*pc_idx)++;
}

void eval_stop(vmc_t *vmc, INT *pc_idx) {
  (void)vmc;
  (void)pc_idx;
}

void eval_app(vmc_t *vmc, INT *pc_idx) {

  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  heap_index closure_address = e.value; // TODO: should we do a pointer check here?
  cam_value_t val = heap_fst(&vmc->heap, closure_address);
  cam_value_t label = heap_snd(&vmc->heap, closure_address);
  heap_index hi = heap_allocate(&vmc->heap);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  }
  heap_set(&vmc->heap, hi, val, hold_reg);
  cam_value_t new_env_pointer =
    { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
  vmc->vm.env = new_env_pointer;


  //jump to label
  INT jump_address = (*pc_idx) + 1; // see Jump convention at the top
  cam_value_t j_add = { .value = (UINT)jump_address };
  int j = stack_push(&vmc->vm.stack, j_add);
  if(j == 0){
    DEBUG_PRINT(("Stack push has failed"));
    *pc_idx = -1;
    return;
  }
  *pc_idx = (INT)label.value;

}

void eval_return(vmc_t *vmc, INT *pc_idx) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  *pc_idx = hold_reg.value;
}

void eval_call(vmc_t *vmc, INT *pc_idx) {
  uint16_t label = get_label(vmc, pc_idx);
  INT jump_address = (*pc_idx) + 3; // see Jump convention at the top
  cam_value_t j_add = { .value = (UINT)jump_address };
  int i = stack_push(&vmc->vm.stack, j_add);
  if(i == 0){
    DEBUG_PRINT(("Stack push has failed"));
    *pc_idx = -1;
    return;
  }
  *pc_idx = (INT)label;
}

void eval_goto(vmc_t *vmc, INT *pc_idx) {
  uint16_t label = get_label(vmc, pc_idx);
  // GOTO doesn't store jump address on stack
  *pc_idx = (INT)label;
}

void eval_gotofalse(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  vmc->vm.env = hold_reg;
  if ((e.value & 1) == 0){ // NOT SET; FALSE
    eval_goto(vmc, pc_idx);
  } else { // TRUE
    *pc_idx = (*pc_idx) + 3;
  }
}

void eval_switch(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  heap_index closure_address = e.value; // TODO: should we do a pointer check here?
  cam_value_t tag_heap = heap_fst(&vmc->heap, closure_address);
  cam_value_t val = heap_snd(&vmc->heap, closure_address);
  INT switch_size_idx = (*pc_idx) + 1;
  uint8_t switch_size = vmc->code_memory[switch_size_idx];

  int label_to_jump = -1;
  for(uint8_t i = (switch_size_idx + 1); i <= (switch_size_idx + (switch_size * 4)); i+=4){
    INT tag_idx1 = i;
    INT tag_idx2 = i + 1;
    uint16_t tag =
      (vmc->code_memory[tag_idx1] << 8) | vmc->code_memory[tag_idx2]; // merge 2 bytes

    INT lab_idx1 = i + 2;
    INT lab_idx2 = i + 3;
    uint16_t label =
      (vmc->code_memory[lab_idx1] << 8) | vmc->code_memory[lab_idx2]; // merge 2 bytes


    if(tag_heap.value == (UINT)tag){
      label_to_jump = label;
      break;
    }
  }
  if(label_to_jump == -1){
    DEBUG_PRINT(("Tag %u not found while switching", tag_heap.value));
    *pc_idx = -1;
    return;
  }

  heap_index hi = heap_allocate(&vmc->heap);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  }
  cam_value_t env_pointer =
    { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
  vmc->vm.env = env_pointer;
  heap_set(&vmc->heap, hi, hold_reg, val);


  //jump to label
  INT shift_pc_by =
    1 + //switch op_code
    1 + // size parameter 1 byte
    (4 * switch_size); // 4 bytes (2 for label and 2 for tag)

  INT jump_address = (*pc_idx) + shift_pc_by; // see Jump convention at the top
  cam_value_t j_add = { .value = (UINT)jump_address };
  int j = stack_push(&vmc->vm.stack, j_add);
  if(j == 0){
    DEBUG_PRINT(("Stack push has failed"));
    *pc_idx = -1;
    return;
  }
  *pc_idx = (INT)label_to_jump;
}

void eval_abs(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->vm.env;
  INT signed_i = (INT)e.value;
  INT abs_i = abs(signed_i);
  cam_value_t v = { .value = (UINT)abs_i, .flags = 0};
  vmc->vm.env = v;
  (*pc_idx)++;
}

void eval_neg(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->vm.env;
  UINT i = e.value;
  INT j = -i; // XXX: might cause underflow for large uints
  cam_value_t v = { .value = (UINT)j, .flags = 0};
  vmc->vm.env = v;
  (*pc_idx)++;

}

void eval_not(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->vm.env;
  UINT i = e.value;
  UINT j = i ^ 1;
  cam_value_t v = { .value = j, .flags = 0};
  vmc->vm.env = v;
  (*pc_idx)++;

}

void eval_dec(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->vm.env;
  UINT i = e.value;
  INT j = i - 1; // XXX: casting might cause issues for uint when outside int range
                 // dec should work with signed ints
  cam_value_t v = { .value = (UINT)j, .flags = 0};
  vmc->vm.env = v;
  (*pc_idx)++;

}

void eval_add_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value + e.value };
  vmc->vm.env = final_value;
}

void eval_mul_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value * e.value };
  vmc->vm.env = final_value;
}

void eval_min_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value - e.value };
  vmc->vm.env = final_value;
}

void eval_add_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
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
}

void eval_mul_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
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
}

void eval_min_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
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
}


void eval_addf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
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
}

void eval_mulf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  float temp1;
  float temp2;
  float temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 * temp2;
  memcpy(&final_value.value, &temp3, sizeof(float));
  vmc->vm.env = final_value;
}

void eval_minf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  float temp1;
  float temp2;
  float temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 - temp2;
  memcpy(&final_value.value, &temp3, sizeof(float));
  vmc->vm.env = final_value;
}

void eval_gt_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value > e.value };
  vmc->vm.env = final_value;
}

void eval_lt_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value < e.value };
  vmc->vm.env = final_value;
}

void eval_ge_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value >= e.value };
  vmc->vm.env = final_value;
}

void eval_le_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value <= e.value };
  vmc->vm.env = final_value;
}

void eval_gt_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  INT temp1;
  INT temp2;
  INT temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 > temp2;
  memcpy(&final_value.value, &temp3, sizeof(INT));
  vmc->vm.env = final_value;
}

void eval_lt_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  INT temp1;
  INT temp2;
  INT temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 < temp2;
  memcpy(&final_value.value, &temp3, sizeof(INT));
  vmc->vm.env = final_value;
}

void eval_ge_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  INT temp1;
  INT temp2;
  INT temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 >= temp2;
  memcpy(&final_value.value, &temp3, sizeof(INT));
  vmc->vm.env = final_value;
}

void eval_le_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  INT temp1;
  INT temp2;
  INT temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 <= temp2;
  memcpy(&final_value.value, &temp3, sizeof(INT));
  vmc->vm.env = final_value;

}

void eval_gtf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  float temp1;
  float temp2;
  float temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 > temp2;
  memcpy(&final_value.value, &temp3, sizeof(float));
  vmc->vm.env = final_value;
}

void eval_ltf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  float temp1;
  float temp2;
  float temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 < temp2;
  memcpy(&final_value.value, &temp3, sizeof(float));
  vmc->vm.env = final_value;
}

void eval_gef(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  float temp1;
  float temp2;
  float temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 >= temp2;
  memcpy(&final_value.value, &temp3, sizeof(float));
  vmc->vm.env = final_value;
}

void eval_lef(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  float temp1;
  float temp2;
  float temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 <= temp2;
  memcpy(&final_value.value, &temp3, sizeof(float));
  vmc->vm.env = final_value;
}


// Equality on base types

void eval_eq_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value == e.value };
  vmc->vm.env = final_value;
}

void eval_eq_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  INT temp1;
  INT temp2;
  INT temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 == temp2;
  memcpy(&final_value.value, &temp3, sizeof(INT));
  vmc->vm.env = final_value;
}

void eval_eqf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  float temp1;
  float temp2;
  float temp3;
  cam_register_t final_value = { .flags = 0, .value = 0 };
  memcpy(&temp1, &hold_reg.value, sizeof(UINT));
  memcpy(&temp2, &e.value, sizeof(UINT));
  temp3 = temp1 == temp2;
  memcpy(&final_value.value, &temp3, sizeof(float));
  vmc->vm.env = final_value;

}

void eval_eq_bool(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->vm.env; // bool represented using uint
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&vmc->vm.stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value == e.value };
  vmc->vm.env = final_value;
}
