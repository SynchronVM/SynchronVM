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

bool eval_fst_test(){
  heap_cell_t hc = { .fst = 5 };
  heap_t hp = { .cells = &hc };
  vmc_t vmc = { .heap = hp };
  int i = eval_fst(&vmc, NULL);
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

  bool t1 = eval_fst_test();

  if (t1) {
    printf("All eval unit tests passed\n");
    return 0;
  }

  printf("Some tests have failed\n");
  return 1;
}
