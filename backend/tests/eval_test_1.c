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

int main(int argc, char **argv) {
  (void)argc;
  (void)argv;
  int total = 0;


  bool t1 = eval_fst_test();
  bool t2 = eval_snd_test();
  if (t1) {
    printf("eval_fst unit test passed\n");
    total++;
  } else {
    printf("eval_fst unit test failed\n");
  }
  if (t2) {
    printf("eval_snd unit test passed\n");
    total++;
  } else {
    printf("eval_snd unit test failed\n");
  }

  printf("Passed total : %d tests\n", total);
  return 1;
}
