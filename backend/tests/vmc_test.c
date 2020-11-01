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

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
/* #include <string.h> */ // has memcpy


bool vmc_run_1_test(){
  /* Stack memory */
  uint8_t *sm = malloc(256);

  /* HEAP */
  heap_t h = { .size_bytes = 0 };
  uint8_t *hm = malloc(1024);
  int h_init = heap_init(&h, hm, 1024);
  if (h_init == 0){
    printf("Heap initialization has failed");
    free(hm);
    free(sm);
    return false;
  }

  /*
    let a = 5 in
    (a * a)
   */
  uint8_t code [] =
    { 254,237,202,254,1,0,1,0,0,0,5,0,0,0,0,0,0,0,16,4,6,0,0,9,4,2,0,12,5,2,0,12,25,13,12 };
  vmc_t container = { .heap = h, .code_memory = code, .stack_memory = sm};

  int run = vmc_run(&container);

  if (run == -1){
    printf("vmc_run has failed");
    free(hm);
    free(sm);
    return false;
  }

  free(hm);
  free(sm);

  if(container.context.env.value == 25){
    return true;
  } else {
    return false;
  }
}

bool vmc_run_2_test(){
  /* Stack memory */
  uint8_t *sm = malloc(256);

  /* HEAP */
  heap_t h = { .size_bytes = 0 };
  uint8_t *hm = malloc(1024);
  int h_init = heap_init(&h, hm, 1024);
  if (h_init == 0){
    printf("Heap initialization has failed");
    free(hm);
    free(sm);
    return false;
  }

  /*
    let foo = let m = 11
               in 3
      in let baz = foo + 2
           in (baz + 4)

  */
  uint8_t code [] =
    { 254,237,202,254,1,0,3,0,0,0,3,0,0,0,2,0,0,0,4,0,0,0,0,0,0,0,36,4,4,10,0,58,9,6,0,0,9,4,4,2,0,12,5,6,0,1,24,9,4,2,0,12,5,6,0,2,24,13,2,0,12,15,12 };

  vmc_t container = { .heap = h, .code_memory = code, .stack_memory = sm};

  int run = vmc_run(&container);

  if (run == -1){
    printf("vmc_run has failed");
    free(hm);
    free(sm);
    return false;
  }

  free(hm);
  free(sm);

  if(container.context.env.value == 9){
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


  bool t1 = vmc_run_1_test();
  test_stat("vmc_run_1", &total, t1);
  bool t2 = vmc_run_2_test();
  test_stat("vmc_run_2", &total, t2);

  printf("Passed total : %d/%d tests\n", total, 2);
}
