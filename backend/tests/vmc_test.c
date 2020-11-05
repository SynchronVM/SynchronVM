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
#include <heap.h>
/* #include <string.h> */ // has memcpy

bool prepare_container(vmc_t *container, int stack_memory_size, int heap_memory_size, uint8_t *code){
  /* Stack memory */
  uint8_t *sm = malloc(stack_memory_size);

  /* HEAP */
  heap_t h = { .size_bytes = 0 };
  uint8_t *hm = malloc(heap_memory_size);
  int h_init = heap_init(&h, hm, heap_memory_size);
  if (h_init == 0){
    printf("Heap initialization has failed");
    free(hm);
    return false;
  }
  container->heap = h;
  container->code_memory = code;
  container->stack_memory = sm;

  // Initialize parent context stack
  cam_stack_t s = { .size = 0 };
  int s_init = stack_init(&s, container->stack_memory, stack_memory_size);
  if (s_init == 0){
    printf("Stack initialization has failed");
    free(sm);
    return false; // indicates error
  }

  container->context.stack = s; /* Parent context uses the full stack memory for now */

  return true;

}


bool vmc_run_1_test(){
  /*
    let a = 5 in
    (a * a)
   */
  uint8_t code [] =
    { 254,237,202,254,1,0,1,0,0,0,5,0,0,0,0,0,0,0,16,4,6,0,0,9,4,2,0,12,5,2,0,12,25,13,12 };


  vmc_t container;
  bool p = prepare_container(&container, 256, 1024, code);
  if(!p){
    return false;
  }

  int run = vmc_run(&container);

  if (run == -1){
    printf("vmc_run has failed");
    /* free(&container.heap); */
    free(container.stack_memory);
    return false;
  }

  /* free(&container.heap); */
  free(container.stack_memory);

  if(container.context.env.value == 25){
    return true;
  } else {
    return false;
  }
}

bool vmc_run_2_test(){

  /*
    let foo = let m = 11
               in 3
      in let baz = foo + 2
           in (baz + 4)

  */
  uint8_t code [] =
    { 254,237,202,254,1,0,3,0,0,0,3,0,0,0,2,0,0,0,4,0,0,0,0,0,0,0,36,4,4,10,0,58,9,6,0,0,9,4,4,2,0,12,5,6,0,1,24,9,4,2,0,12,5,6,0,2,24,13,2,0,12,15,12 };

  vmc_t container;
  bool p = prepare_container(&container, 256, 1024, code);
  if(!p){
    return false;
  }

  int run = vmc_run(&container);

  if (run == -1){
    printf("vmc_run has failed");
    free(container.stack_memory);
    return false;
  }

  free(container.stack_memory);

  if(container.context.env.value == 9){
    return true;
  } else {
    return false;
  }
}

bool vmc_run_3_test(){

  /*
    ((\x -> x + 4) 3)
  */
  uint8_t code [] =
    { 254,237,202,254,1,0,2,0,0,0,3,0,0,0,4,0,0,0,0,0,0,0,21,4,6,0,0,5,10,0,33,14,13,4,2,0,12,5,6,0,1,24,15,12 };

  vmc_t container;
  bool p = prepare_container(&container, 256, 1024, code);
  if(!p){
    return false;
  }

  int run = vmc_run(&container);

  if (run == -1){
    printf("vmc_run has failed");
    free(container.stack_memory);
    return false;
  }

  free(container.stack_memory);

  if(container.context.env.value == 7){
    return true;
  } else {
    return false;
  }
}

bool vmc_run_4_test(){

  /*
    let foo = let m = 11
               in \x -> x
    in let baz = foo 2
        in (baz + 4)


  */
  uint8_t code [] =
    { 254,237,202,254,1,0,3,0,0,0,11,0,0,0,2,0,0,0,4,0,0,0,0,0,0,0,36,4,4,6,0,0,9,10,0,58,9,4,4,6,0,1,5,2,0,12,14,9,4,2,0,12,5,6,0,2,24,13,2,0,12,15,12 };


  vmc_t container;
  bool p = prepare_container(&container, 256, 1024, code);
  if(!p){
    return false;
  }

  int run = vmc_run(&container);

  if (run == -1){
    printf("vmc_run has failed");
    free(container.stack_memory);
    return false;
  }

  free(container.stack_memory);

  if(container.context.env.value == 6){
    return true;
  } else {
    return false;
  }
}

bool vmc_run_5_test(){
  /*
    letrec even = \n -> if (n == 0) then true else not (even (n - 1))
    in even 1
  */
  uint8_t code [] =
    { 254,237,202,254,1,0,2,0,0,0,56,0,0,0,0,0,0,0,0,0,0,0,52,4,6,0,0,5,3,0,16,0,36,12,14,13,10,0,40,15,4,4,2,0,12,5,6,0,1,32,18,0,58,7,1,17,0,72,4,2,0,12,23,5,3,1,16,0,36,12,14,22,12,15,12 };

  vmc_t container;
  bool p = prepare_container(&container, 512, 1024, code); // requires more stack as well as GC
  if(!p){
    return false;
  }

  int run = vmc_run(&container);

  if (run == -1){
    printf("vmc_run has failed");
    free(container.stack_memory);
    return false;
  }

  free(container.stack_memory);

  if(container.context.env.value == 1){ //env register contains True?
    return true;
  } else {
    return false;
  }
}

bool vmc_run_6_test(){
  /*
    letrec not = \b -> if b == True then False else True
          even = \n -> if (n == 0) then true else not (even (n - 1))
        in even 53
  */
  uint8_t code [] =
    { 254,237,202,254,1,0,2,0,0,0,53,0,0,0,0,0,0,0,0,0,0,0,85,4,6,0,0,5,3,0,16,0,40,12,14,13,10,0,86,15,10,0,44,15,4,4,2,0,12,5,6,0,1,32,18,0,62,7,1,17,0,84,4,4,2,0,12,23,5,3,1,16,0,40,12,14,5,3,1,16,0,36,12,14,12,15,4,4,2,0,12,5,7,1,32,18,0,103,7,0,17,0,105,7,1,12,15,12 };

  vmc_t container;
  bool p = prepare_container(&container, 1024, 1024, code); // requires even more stack than test 5; candidate for tail recursion
  if(!p){
    return false;
  }

  int run = vmc_run(&container);


  if (run == -1){
    printf("vmc_run has failed");
    free(container.stack_memory);
    return false;
  }

  free(container.stack_memory);

  if(container.context.env.value == 0){ //env register contains False?
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
  bool t3 = vmc_run_3_test();
  test_stat("vmc_run_3", &total, t3);
  bool t4 = vmc_run_4_test();
  test_stat("vmc_run_4", &total, t4);
  bool t5 = vmc_run_5_test();
  test_stat("vmc_run_5", &total, t5);
  bool t6 = vmc_run_6_test();
  test_stat("vmc_run_6", &total, t6);

  printf("Passed total : %d/%d tests\n", total, 6);
}
