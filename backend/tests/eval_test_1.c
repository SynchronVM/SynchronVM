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
#include <stdlib.h>
#include <stdio.h>

int eval_fst(vmc_t *vmc, uint8_t *bc_rest);
int eval_snd(vmc_t *vmc, uint8_t *bc_rest);
int eval_push(vmc_t *vmc, uint8_t *bc_rest);
int eval_cons(vmc_t *vmc, uint8_t *bc_rest);
int eval_cur(vmc_t *vmc, uint8_t *bc_rest);

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
  int i = eval_fst(&vmc, NULL);
  (void)i;
  if(vmc.vm.env.value == 5){
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
  int i = eval_snd(&vmc, NULL);
  (void)i;
  if(vmc.vm.env.value == 5){
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
    return false;
  }
  VM_t mockvm = { .env = cv, .stack = s };
  vmc_t vmc = { .vm = mockvm };

  cam_register_t dummyreg = { .value = 0 };
  int i = eval_push(&vmc, NULL);
  if (i == -1){
    printf("push operation has failed");
    return false;
  }
  int j = stack_pop(&vmc.vm.stack, &dummyreg);
  if (j == 0){
    printf("Stack pop has failed");
    return false;
  }
  if(dummyreg.value == 10){
    free(m);
    return true;
  } else {
    free(m);
    return false;
  }
}

bool eval_cons_test(){

  //Value to be held in the environment register
  cam_value_t v2 = { .value = 10, .flags = 0 };

  //Initializing a mock stack
  cam_value_t v1 = { .value = 20, .flags = 0 };
  cam_stack_t s = { .size = 0 };
  uint8_t *m = malloc(256);
  int s_init = stack_init(&s, m, 256);
  if (s_init == 0){
    printf("Stack initialization has failed");
    return false;
  }
  int y = stack_push(&s, v1);
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

  VM_t mockvm = { .env = v2, .stack = s };
  vmc_t vmc = { .vm = mockvm, .heap = h};
  int i = eval_cons(&vmc, NULL);
  if(i == -1){
    printf("cons operation has failed\n");
    return false;
  }

  /* heap_show(&vmc.heap, 3); */ //Debugging
  cam_value_t fst = heap_fst(&vmc.heap, (INT)vmc.vm.env.value);
  cam_value_t snd = heap_snd(&vmc.heap, (INT)vmc.vm.env.value);
  if(fst.value == 20 && snd.value == 10){
    free(m); free(hm);
    return true;
  } else {
    free(m); free(hm);
    return false;
  }
}

/* TODO: This test should be made such that the */
/* environment register points to a thunk on the  */
/* heap and then eval_cur should be evaluated */
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
  vmc_t vmc = { .vm = mockvm, .heap = h};
  uint8_t code [] = { 1 };
  int i = eval_cur(&vmc, code);
  if(i == -1){
    printf("cur l operation has failed\n");
    return false;
  }

  //heap_show(&vmc.heap, 3);
  cam_value_t fst = heap_fst(&vmc.heap, (INT)vmc.vm.env.value);
  cam_value_t snd = heap_snd(&vmc.heap, (INT)vmc.vm.env.value);
  if(fst.value == 10 && snd.value == 1){
    free(hm);
    return true;
  } else {
    free(hm);
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

  printf("Passed total : %d tests\n", total);
  return 1;
}
