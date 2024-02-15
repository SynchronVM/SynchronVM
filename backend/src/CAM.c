/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Joel Svensson, Abhiroop Sarkar 				  */
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

#include <SVM_DEBUG.h>

#include <CAM.h>
#include <VMC.h>
#include <stdlib.h>
#include <string.h>

#include <ll/ll_driver.h>
#include <platform.h>

/* Combinators */

#define COMB 4294967295


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
/* Optimised instructions */
void eval_move(vmc_t *vmc, INT *pc_idx);
void eval_pop (vmc_t *vmc, INT *pc_idx);
void eval_snoc(vmc_t *vmc, INT *pc_idx);
void eval_comb(vmc_t *vmc, INT *pc_idx);
void eval_gotoifalse(vmc_t *vmc, INT *pc_idx);
void eval_switchi   (vmc_t *vmc, INT *pc_idx);
void eval_callrts   (vmc_t *vmc, INT *pc_idx);
void eval_appf(vmc_t *vmc, INT *pc_idx);



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
    eval_eq_unsignedi,
    eval_ge_unsignedi,
    eval_le_unsignedi,
    eval_gtf,
    eval_ltf,
    eval_eqf,
    eval_gef,
    eval_lef,
    eval_eq_bool,
    eval_move,
    eval_pop,
    eval_snoc,
    eval_comb,
    eval_gotoifalse,
    eval_switchi,
    eval_callrts,  // 0x37 : 55
    eval_appf      // 0x38 : 56
  };


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

static bool is_all_contexts_stopped(vmc_t *vmc){
  bool start = false;
  for(int i = 0; i < VMC_MAX_CONTEXTS; i++){
    start = start | vmc->context_used[i];
  }
  return !start;
}

void eval_fst(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_value_t v = heap_fst(&vmc->heap, (heap_index)e.value);
  vmc->contexts[vmc->current_running_context_id].env = v;
}

void eval_snd(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_value_t v = heap_snd(&vmc->heap, (heap_index)e.value);
  vmc->contexts[vmc->current_running_context_id].env = v;
}


void eval_acc(vmc_t *vmc, INT *pc_idx) {
  INT n_idx = (*pc_idx) + 1;
  uint8_t acc_n = vmc->code_memory[n_idx];
  for(unsigned int i = 0; i < acc_n; i++){
    cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
    cam_value_t v = heap_fst(&vmc->heap, (heap_index)e.value);
    vmc->contexts[vmc->current_running_context_id].env = v;
  }
  cam_value_t v = heap_snd(&vmc->heap, (heap_index)vmc->contexts[vmc->current_running_context_id].env.value);
  vmc->contexts[vmc->current_running_context_id].env = v;
  *pc_idx = (*pc_idx) + 2;
}

void eval_rest(vmc_t *vmc, INT *pc_idx)  {
  INT n_idx = (*pc_idx) + 1;
  uint8_t acc_n = vmc->code_memory[n_idx];
  for(unsigned int i = 0; i < acc_n; i++){
    cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
    cam_value_t v = heap_fst(&vmc->heap, (heap_index)e.value);
    vmc->contexts[vmc->current_running_context_id].env = v;
  }
  *pc_idx = (*pc_idx) + 2;
}

void eval_push(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  int i = stack_push(&vmc->contexts[vmc->current_running_context_id].stack, e);
  if(i == 0){
    DEBUG_PRINT(("Stack push has failed"));
    *pc_idx = -1;
    return;
  }
  (*pc_idx)++;
}

void eval_swap(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  int j = stack_push(&vmc->contexts[vmc->current_running_context_id].stack, e);
  if(j == 0){
    DEBUG_PRINT(("Stack push has failed"));
    *pc_idx = -1;
    return;
  }
  vmc->contexts[vmc->current_running_context_id].env = hold_reg;
  (*pc_idx)++;
}

void eval_loadi(vmc_t *vmc, INT *pc_idx) {
  INT int_idx1 = (*pc_idx) + 1;
  INT int_idx2 = (*pc_idx) + 2;
  uint16_t int_idx =
    (vmc->code_memory[int_idx1] << 8) | vmc->code_memory[int_idx2]; // merge 2 bytes
  INT int_pool_offset = 7; //TODO: Should we verify the int pool size here?
  INT i_idx = int_pool_offset + 4 * int_idx; // each int 4 bytes wide
  uint8_t byte0 = vmc->code_memory[i_idx];
  uint8_t byte1 = vmc->code_memory[i_idx + 1];
  uint8_t byte2 = vmc->code_memory[i_idx + 2];
  uint8_t byte3 = vmc->code_memory[i_idx + 3];
  INT i = (byte0 << 24) | (byte1 << 16) | (byte2 << 8) | byte3;
  cam_value_t v = { .value = (UINT)i, .flags = 0};
  vmc->contexts[vmc->current_running_context_id].env = v;
  *pc_idx = (*pc_idx) + 3;
}

void eval_loadb(vmc_t *vmc, INT *pc_idx) {
  INT bool_idx = (*pc_idx) + 1;
  uint8_t bool_val = vmc->code_memory[bool_idx];
  cam_value_t v = { .value = (UINT)bool_val, .flags = 0};
  vmc->contexts[vmc->current_running_context_id].env = v;
  *pc_idx = (*pc_idx) + 2;
}

void eval_clear(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_value_t empty_tuple = { .value = 0, .flags = 0 };
  vmc->contexts[vmc->current_running_context_id].env = empty_tuple;
}

void eval_cons(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  heap_index hi = vmc_heap_alloc_withGC(vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  } else {
    // Assuming we have space for atleast one tuple
    // Do we check this as well?
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->contexts[vmc->current_running_context_id].env = env_pointer;
    heap_set(&vmc->heap, hi, hold_reg, e);
  }
}

void eval_cur(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  uint16_t label = get_label(vmc, pc_idx);
  cam_value_t cam_label =
    { .value = (UINT)label, .flags = 0 };
  heap_index hi = vmc_heap_alloc_withGC(vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  } else {
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->contexts[vmc->current_running_context_id].env = env_pointer;
    heap_set(&vmc->heap, hi, e, cam_label);
    *pc_idx = (*pc_idx) + 3;
  }
}

void eval_pack(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  uint16_t tag = get_tag(vmc, pc_idx);
  cam_value_t cam_tag =
    { .value = (UINT)tag, .flags = 0 };
  heap_index hi = vmc_heap_alloc_withGC(vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  } else {
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->contexts[vmc->current_running_context_id].env = env_pointer;
    heap_set(&vmc->heap, hi, cam_tag, e);
    *pc_idx = (*pc_idx) + 3;
  }
}

void eval_skip(vmc_t *vmc, INT *pc_idx) {
  (void)vmc;
  (*pc_idx)++;
}

void eval_stop(vmc_t *vmc, INT *pc_idx) {
  (void)pc_idx;
  vmc->context_used[vmc->current_running_context_id] = false;
  int i = dispatch(vmc);
  if(i == -1)
    DEBUG_PRINT(("Ready Queue is empty\n"));
  if(is_all_contexts_stopped(vmc)){
    vmc->all_contexts_stopped = true;
  }

}

void eval_app(vmc_t *vmc, INT *pc_idx) {

  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }

  heap_index closure_address = e.value; // TODO: should we do a pointer check here?
                                        // closure or combinator, the if checks that

  cam_value_t heap_f = heap_fst(&vmc->heap, closure_address);
  cam_value_t heap_s = heap_snd(&vmc->heap, closure_address);

  if(heap_s.value == COMB){ // if combinator

    cam_value_t label = heap_f;

    vmc->contexts[vmc->current_running_context_id].env = hold_reg;


    //jump to label
    INT jump_address = (*pc_idx) + 1; // see Jump convention at the top
    cam_value_t j_add = { .value = (UINT)jump_address };
    int j = stack_push(&vmc->contexts[vmc->current_running_context_id].stack, j_add);
    if(j == 0){
      DEBUG_PRINT(("Stack push has failed"));
      *pc_idx = -1;
      return;
    }
    *pc_idx = (INT)label.value;

  } else { // not a combinator but a closure

    cam_value_t val = heap_f;
    cam_value_t label = heap_s;

    heap_index hi = vmc_heap_alloc_withGC(vmc);
    if(hi == HEAP_NULL){
      DEBUG_PRINT(("Heap allocation has failed"));
      *pc_idx = -1;
      return;
    }
    heap_set(&vmc->heap, hi, val, hold_reg);
    cam_value_t new_env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->contexts[vmc->current_running_context_id].env = new_env_pointer;


    //jump to label
    INT jump_address = (*pc_idx) + 1; // see Jump convention at the top
    cam_value_t j_add = { .value = (UINT)jump_address };
    int j = stack_push(&vmc->contexts[vmc->current_running_context_id].stack, j_add);
    if(j == 0){
      DEBUG_PRINT(("Stack push has failed"));
      *pc_idx = -1;
      return;
    }
    *pc_idx = (INT)label.value;

  }

}

void eval_return(vmc_t *vmc, INT *pc_idx) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  int i = stack_push(&vmc->contexts[vmc->current_running_context_id].stack, j_add);
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
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  vmc->contexts[vmc->current_running_context_id].env = hold_reg;
  if ((e.value & 1) == 0){ // NOT SET; FALSE
    eval_goto(vmc, pc_idx);
  } else { // TRUE
    *pc_idx = (*pc_idx) + 3;
  }
}

void eval_switch(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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

  for(uint32_t i = (switch_size_idx + 1); i <= (switch_size_idx + (switch_size * 4)); i+=4){
    INT tag_idx1 = i;
    INT tag_idx2 = i + 1;
    uint16_t tag =
      (vmc->code_memory[tag_idx1] << 8) | vmc->code_memory[tag_idx2]; // merge 2 bytes

    INT lab_idx1 = i + 2;
    INT lab_idx2 = i + 3;
    uint16_t label =
      (vmc->code_memory[lab_idx1] << 8) | vmc->code_memory[lab_idx2]; // merge 2 bytes


    if(tag_heap.value == (UINT)tag ||
       (UINT)tag == 65535){ //wildcard check; wildcard tag = max(uint16_t) = 65535
      label_to_jump = label;
      break;
    }
  }
  if(label_to_jump == -1){
    DEBUG_PRINT(("Tag %u not found while switching", tag_heap.value));
    *pc_idx = -1;
    return;
  }

  heap_index hi = vmc_heap_alloc_withGC(vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  }
  cam_value_t env_pointer =
    { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
  vmc->contexts[vmc->current_running_context_id].env = env_pointer;
  heap_set(&vmc->heap, hi, hold_reg, val);

  //goto label
  *pc_idx = (INT)label_to_jump;
}

void eval_abs(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  INT signed_i = (INT)e.value;
  INT abs_i = abs(signed_i);
  cam_value_t v = { .value = (UINT)abs_i, .flags = 0};
  vmc->contexts[vmc->current_running_context_id].env = v;
  (*pc_idx)++;
}

void eval_neg(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  UINT i = e.value;
  INT j = -i; // XXX: might cause underflow for large uints
  cam_value_t v = { .value = (UINT)j, .flags = 0};
  vmc->contexts[vmc->current_running_context_id].env = v;
  (*pc_idx)++;

}

void eval_not(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  UINT i = e.value;
  UINT j = i ^ 1;
  cam_value_t v = { .value = j, .flags = 0};
  vmc->contexts[vmc->current_running_context_id].env = v;
  (*pc_idx)++;

}

void eval_dec(vmc_t *vmc, INT *pc_idx) {
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  UINT i = e.value;
  INT j = i - 1; // XXX: casting might cause issues for uint when outside int range
                 // dec should work with signed ints
  cam_value_t v = { .value = (UINT)j, .flags = 0};
  vmc->contexts[vmc->current_running_context_id].env = v;
  (*pc_idx)++;

}

void eval_add_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value + e.value };
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_mul_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value * e.value };
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_min_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value - e.value };
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_add_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_mul_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_min_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}


void eval_addf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_mulf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_minf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_gt_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value > e.value };
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_lt_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value < e.value };
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_ge_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value >= e.value };
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_le_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value <= e.value };
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_gt_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_lt_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_ge_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_le_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;

}

void eval_gtf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_ltf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_gef(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_lef(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}


// Equality on base types

void eval_eq_unsignedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value == e.value };
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_eq_signedi(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_eqf(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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
  vmc->contexts[vmc->current_running_context_id].env = final_value;

}

void eval_eq_bool(vmc_t *vmc, INT *pc_idx) {
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env; // bool represented using uint
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  cam_register_t final_value =
    { .flags = 0, .value = hold_reg.value == e.value };
  vmc->contexts[vmc->current_running_context_id].env = final_value;
}

void eval_move(vmc_t *vmc, INT *pc_idx){
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  int i = stack_push(&vmc->contexts[vmc->current_running_context_id].stack, e);
  if(i == 0){
    DEBUG_PRINT(("Stack push has failed"));
    *pc_idx = -1;
    return;
  }

  cam_value_t empty_tuple = { .value = 0, .flags = 0 };
  vmc->contexts[vmc->current_running_context_id].env = empty_tuple;

  (*pc_idx)++;

}

void eval_pop (vmc_t *vmc, INT *pc_idx){
  cam_register_t r;
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &r);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  vmc->contexts[vmc->current_running_context_id].env = r;

  (*pc_idx)++;
}
void eval_snoc(vmc_t *vmc, INT *pc_idx){
  (*pc_idx)++;
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
  cam_register_t hold_reg;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    *pc_idx = -1;
    return;
  }
  heap_index hi = vmc_heap_alloc_withGC(vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  } else {
    // Assuming we have space for atleast one tuple
    // Do we check this as well?
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->contexts[vmc->current_running_context_id].env = env_pointer;
    heap_set(&vmc->heap, hi, e, hold_reg);
  }

}
void eval_comb(vmc_t *vmc, INT *pc_idx){

  uint16_t label = get_label(vmc, pc_idx);
  cam_value_t cam_label =
    { .value = (UINT)label, .flags = 0 };
  heap_index hi = vmc_heap_alloc_withGC(vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    *pc_idx = -1;
    return;
  } else {
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    vmc->contexts[vmc->current_running_context_id].env = env_pointer;


    // This value is used to demarcate a heap cell as
    // storing a combinator value rather than a closure
    cam_value_t dummy_val = { .value = COMB };


    heap_set(&vmc->heap, hi, cam_label, dummy_val);

    *pc_idx = (*pc_idx) + 3;
  }

}
void eval_gotoifalse(vmc_t *vmc, INT *pc_idx){
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;

  if ((e.value & 1) == 0){ // NOT SET; FALSE
    eval_goto(vmc, pc_idx);
  } else { // TRUE
    *pc_idx = (*pc_idx) + 3;
  }

}

void eval_switchi(vmc_t *vmc, INT *pc_idx){

  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;

  heap_index tag_val_pair = e.value;
  cam_value_t tag_heap = heap_fst(&vmc->heap, tag_val_pair);
  cam_value_t val      = heap_snd(&vmc->heap, tag_val_pair);
  INT switch_size_idx = (*pc_idx) + 1;
  uint8_t switch_size = vmc->code_memory[switch_size_idx];

  int label_to_jump = -1;
  for(uint32_t i = (switch_size_idx + 1); i <= (switch_size_idx + (switch_size * 4)); i+=4){
    INT tag_idx1 = i;
    INT tag_idx2 = i + 1;
    uint16_t tag =
      (vmc->code_memory[tag_idx1] << 8) | vmc->code_memory[tag_idx2]; // merge 2 bytes

    INT lab_idx1 = i + 2;
    INT lab_idx2 = i + 3;
    uint16_t label =
      (vmc->code_memory[lab_idx1] << 8) | vmc->code_memory[lab_idx2]; // merge 2 bytes


    if(tag_heap.value == (UINT)tag ||
       (UINT)tag == 65535){ //wildcard check; wildcard tag = max(uint16_t) = 65535
      label_to_jump = label;
      break;
    }
  }
  if(label_to_jump == -1){
    DEBUG_PRINT(("Tag %u not found while switching", tag_heap.value));
    *pc_idx = -1;
    return;
  }

  vmc->contexts[vmc->current_running_context_id].env = val;


  //goto label
  *pc_idx = (INT)label_to_jump;

}

static int handle_spawn(vmc_t *vmc){

  /* IMP:
   * spawn starts a new process with the signature () -> ()
   * When we jump to this process (because of sync) the operation
   * is like processing an APP with the argument (). So the
   * argument () needs to be placed in the environment depending
   * on the cases of a closure [v:l] (snocced in this case) or
   * a combinator [l] (simply place () in the env in this case)
   */
  cam_value_t empty_tuple = { .value = 0, .flags = 0 };



  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;

  heap_index closure_address = e.value;


  cam_value_t heap_f = heap_fst(&vmc->heap, closure_address);
  cam_value_t heap_s = heap_snd(&vmc->heap, closure_address);

  if(heap_s.value == COMB){ // if combinator

    cam_value_t label = heap_f;

    vmc->contexts[vmc->current_running_context_id].env = empty_tuple;

    return spawn(vmc, (uint16_t)label.value); // will place PID in env


  } else { // not a combinator but a closure

    cam_value_t val = heap_f;
    cam_value_t label = heap_s;


    // Put (v, ()) of [v:l] on the env register; Read above why () comes;
    // spawn then copies the content of the env register to
    // the `env` register of the new context

    heap_index hi = vmc_heap_alloc_withGC(vmc);
    if(hi == HEAP_NULL){
      DEBUG_PRINT(("Heap allocation has failed"));
      return -1;
    }
    heap_set(&vmc->heap, hi, val, empty_tuple);
    cam_value_t new_env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };

    vmc->contexts[vmc->current_running_context_id].env = new_env_pointer;

    // Spawn will places the label graveyard address on the stack
    return spawn(vmc, (uint16_t)label.value); // will place PID in env

  }

}

static int handle_channel(vmc_t *vmc){
  UUID chan_id;
  int j = channel(vmc, &chan_id);
  if(j == -1){
    DEBUG_PRINT(("Error initializing a channel \n"));
    return j;
  }
  cam_value_t channel_cam = { .value = (UINT)chan_id, .flags = 0 };
  vmc->contexts[vmc->current_running_context_id].env = channel_cam;
  return 1;
}

static int handle_sendevt(vmc_t *vmc){
  cam_value_t message = vmc->contexts[vmc->current_running_context_id].env;

  cam_register_t hold_reg;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  UUID channel_id = (UUID)hold_reg.value;

  event_t send_evt;
  int j = sendEvt(vmc, &channel_id, message, &send_evt);
  if(j == -1){
    DEBUG_PRINT(("Error with sendEvt \n"));
    return j;
  }

  cam_value_t send_evt_env =
    { .value = (UINT)send_evt, .flags = VALUE_PTR_BIT };
  vmc->contexts[vmc->current_running_context_id].env = send_evt_env;
  return 1;
}

static int handle_recvevt(vmc_t *vmc){
  cam_value_t channel_cam = vmc->contexts[vmc->current_running_context_id].env;

  UUID channel_id = (UUID)channel_cam.value;

  event_t recv_evt;
  int j = recvEvt(vmc, &channel_id, &recv_evt);
  if(j == -1){
    DEBUG_PRINT(("Error with recvEvt \n"));
    return j;
  }

  cam_value_t recv_evt_env =
    { .value = (UINT)recv_evt, .flags = VALUE_PTR_BIT };
  vmc->contexts[vmc->current_running_context_id].env = recv_evt_env;
  return 1;
}

static int handle_sync(vmc_t *vmc){
  cam_value_t event_env = vmc->contexts[vmc->current_running_context_id].env;

  if(event_env.flags != VALUE_PTR_BIT){
    DEBUG_PRINT(("Thread number : %u", vmc->current_running_context_id));
    DEBUG_PRINT(("Pointer not found in the environment register \n"));
    return -1;
  }

  event_t evt = (event_t)event_env.value;

  int j = sync(vmc, &evt);
  if(j == -1){
    DEBUG_PRINT(("Error in synchronisation \n"));
    return j;
  }

  return 1;


}

static int handle_choose(vmc_t *vmc){
  cam_value_t e2 = vmc->contexts[vmc->current_running_context_id].env;

  cam_register_t e1;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &e1);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }

  event_t evt1 = (event_t)e1.value;
  event_t evt2 = (event_t)e2.value;

  event_t final_evt;

  choose(vmc, &evt1, &evt2, &final_evt);

  cam_value_t final_evt_cam =
    { .value = (UINT)final_evt, .flags = VALUE_PTR_BIT };

  vmc->contexts[vmc->current_running_context_id].env = final_evt_cam;

  return 1;

}

static int handle_spawnExternal(vmc_t *vmc){

  //spawnExternal : Channel a -> Int -> ()

  cam_value_t driver_details =
    vmc->contexts[vmc->current_running_context_id].env;

  cam_register_t hold_reg;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  UUID chan_id = (UUID)hold_reg.value;

  if(vmc->drivers[driver_details.value].is_synchronous){
    // synchronous driver like LEDs
    vmc->channels[chan_id].sync_driver_no = (UUID)driver_details.value;
  } else {
    // asynchronous drivers like buttons
    vmc->drivers[driver_details.value].channel_id = chan_id;
  }

  return 1;

}

static int handle_wrap(vmc_t *vmc){

  //wrap : Event a -> (a -> b) -> Event b

  cam_value_t wrapf_ptr =
    vmc->contexts[vmc->current_running_context_id].env;

  cam_register_t hold_reg;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  event_t current_evt = hold_reg.value;

  // IMP: we are guaranteed by the compiler tranformations that
  // current_evt is a base event; so traversal of event not needed

  // get pointer to cam_event_t
  cam_value_t cevt_ptr = heap_fst(&vmc->heap, (heap_index)current_evt);
  // get pointer to base_event_t
  cam_value_t bevt_ptr = heap_fst(&vmc->heap, (heap_index)cevt_ptr.value);
  // set the second of the cell that bevt_ptr is pointing to wrapf_ptr
  heap_set_snd(&vmc->heap, (heap_index)bevt_ptr.value, wrapf_ptr);

  //Place the modified event on the environment
  cam_value_t new_env = { .value = (UINT)current_evt, .flags = VALUE_PTR_BIT };
  vmc->contexts[vmc->current_running_context_id].env = new_env;

  return 1;

}

static int handle_time(vmc_t *vmc){

  //syncT : Time -> Time -> Event a -> a

  cam_value_t hold_reg1 = vmc->contexts[vmc->current_running_context_id].env;

  cam_register_t hold_reg2;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg2);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }

  cam_register_t hold_reg3;
  int j =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg3);
  if(j == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }

  /* Is this a good place to multiply by PLATFORM_TIME_FACTOR? */
  Time baseline = (Time)(hold_reg1.value * PLATFORM_TIME_FACTOR);
  
  Time deadline = (Time)(hold_reg2.value * PLATFORM_TIME_FACTOR);

  // After calling the rts function `time` make sure the
  // env register points to `ev` so that we can `sync` next
  // because the sequence of bytecode will be - ..time; sync...
  vmc->contexts[vmc->current_running_context_id].env = hold_reg3;

  int k = time(vmc, baseline, deadline);
  if(k == -1){
    DEBUG_PRINT(("Error with syncT \n"));
    return k;
  }


  return 1;

  //FUTURE WORK
  //TODO: hold_reg1 and hold_reg2 should contain indices to the int pool
  // find index of 64 bit int baseline and deadline from the int pool

}

void eval_callrts(vmc_t *vmc, INT *pc_idx){
  INT n_idx = (*pc_idx) + 1;
  uint8_t rts_op_no = vmc->code_memory[n_idx];

  // do all operations here
    /* spawn     - 0 */
    /* channel   - 1 */
    /* sendEvt   - 2 */
    /* recvEvt   - 3 */
    /* sync      - 4 */
    /* choose    - 5 */
    /* spawnExternal - 6 */
    /* wrap      - 7 */
    /* time      - 8 */

  int ret_code = -1; 
  switch(rts_op_no){
    case 0:
      ret_code = handle_spawn(vmc);
      break;
    case 1:
      ret_code = handle_channel(vmc);
      break;
    case 2:
      ret_code = handle_sendevt(vmc);
      break;
    case 3:
      ret_code = handle_recvevt(vmc);
      break;
    case 4:
      ret_code = handle_sync(vmc);
      break;
    case 5:
      ret_code = handle_choose(vmc);
      break;
    case 6:
      ret_code = handle_spawnExternal(vmc);
      break;
    case 7:
      ret_code = handle_wrap(vmc);
      break;
    case 8:
      ret_code = handle_time(vmc);
      break;
    default:
      DEBUG_PRINT(("Invalid RTS op number"));
      *pc_idx = ret_code;
      return;
  }
  if(ret_code == -1){
    DEBUG_PRINT(("Error in RTS function"));
    *pc_idx = ret_code;
  } else {

    *pc_idx = (*pc_idx) + 2;
  }
}


void eval_appf(vmc_t *vmc, INT *pc_idx){
  // We set pc_idx = -10 for errors from the APPF bytecode
  INT native_pool_idx_1 = (*pc_idx) + 1;
  INT native_pool_idx_2 = (*pc_idx) + 2;
  uint16_t native_pool_idx =
    ( vmc->code_memory[native_pool_idx_1] << 8)
    | vmc->code_memory[native_pool_idx_2]; // merge 2 bytes
  uint16_t int_pool_count_idx =
    (vmc->code_memory[5] << 8) | vmc->code_memory[6];
  INT cur_idx = 7 + 4 * int_pool_count_idx;

  uint16_t str_pool_count_idx =
    (vmc->code_memory[cur_idx] << 8) | vmc->code_memory[cur_idx + 1];

  cur_idx = cur_idx + 2 + str_pool_count_idx;

  cur_idx += 2; // native pool count = 2 bytes


  INT npool_idx = cur_idx + 4 * native_pool_idx;

  uint8_t byte0    = vmc->code_memory[npool_idx];
  uint8_t byte1    = vmc->code_memory[npool_idx + 1];
  uint8_t num_args = vmc->code_memory[npool_idx + 2];
  // fourth byte unused currently

  uint16_t ffi_func_idx = (byte0 << 8) | byte1;



  cam_value_t res;

  cam_value_t arg_arr[num_args];

  if (num_args == 0){
    res = ffi_arr[ffi_func_idx](arg_arr);
  } else if(num_args == 1){
    arg_arr[0] = vmc->contexts[vmc->current_running_context_id].env;
    res = ffi_arr[ffi_func_idx](arg_arr);
  } else {
    arg_arr[0] = vmc->contexts[vmc->current_running_context_id].env;
    for(int j = 1; j < num_args; j++){
      cam_register_t arg_n;
      int i =
        stack_pop(  &vmc->contexts[vmc->current_running_context_id].stack
                  , &arg_n);
      if(i == 0){
        DEBUG_PRINT(("Stack pop failed inside eval_appf"));
        *pc_idx = -10;
        return;
      }
      arg_arr[j] = arg_n;
    }

    res = ffi_arr[ffi_func_idx](arg_arr);
  }


  vmc->contexts[vmc->current_running_context_id].env = res;


  *pc_idx = (*pc_idx) + 3;

}
