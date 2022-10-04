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
#include <scheduler.h>
/* #include <string.h> */ // has memcpy

int mock_read_message_poll(vmc_t *vmc, svm_msg_t *msg){
  (void)vmc;
  (void)msg;
  return -1;
}

int mock_read_message_block(vmc_t *vmc, svm_msg_t *msg){
  (void)vmc;
  (void)msg;
  return -1;
}

uint32_t mock_message_queue_num_used(vmc_t *vmc) {
  (void)vmc;
  return 0;
}

void mock_debug_print(const char* str, ...){ (void)str; }


static int setup_and_run(vmc_t *container, uint8_t *code, uint32_t c_size){

  int init_status = vmc_init(container, 4);
  if (init_status == -1){
    printf("vmc_init has failed");
    return -1;
  }

  container->code_memory = code;
  container->code_size   = c_size;


  int run = vmc_run(container, mock_debug_print);

  if (run == -1){
    printf("vmc_run has failed");
    return -1;
  }

  int scheduler_status = scheduler(  container
                                     , mock_read_message_poll
                                     , mock_read_message_block
                                     , mock_message_queue_num_used);

  if (scheduler_status == -1){
    printf("scheduler has failed");
    return -1;
  }

  return 1;

}






bool vmc_run_1_test(){
  /*
    let a = 5 in
    (a * a)
   */
  uint8_t code [] =
    { 254,237,202,254,1,0,1,0,0,0,5,0,0,0,0,0,0,0,16,4,6,0,0,9,4,2,0,12,5,2,0,12,25,13,12 };


  vmc_t container;

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_1\n");
    return false;
  }

  if(container.contexts[0].env.value == 25){
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

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_2\n");
    return false;
  }


  if(container.contexts[0].env.value == 9){
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

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_3\n");
    return false;
  }

  if(container.contexts[0].env.value == 7){
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

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_4\n");
    return false;
  }

  if(container.contexts[0].env.value == 6){
    return true;
  } else {
    return false;
  }
}



// NOTE 1: Programs 5, 6, 7 require more stack than the other programs
/*
 * To run them the following parameter need to be changed in VMC.h
 * #define VMC_MAX_CONTEXTS 4
 * #define CONTEXT_STACK_SPACE 256
 *
 * From Program 5:
 * #define VMC_MAX_CONTEXTS 2
 * #define CONTEXT_STACK_SPACE 512
 * 512 * 2 = 1024
 *
 * From Program 6, 7:
 * #define VMC_MAX_CONTEXTS 1
 * #define CONTEXT_STACK_SPACE 1024
 * 1024 * 1 = 1024
 *
 */
bool vmc_run_5_test(){
  /*
    letrec even = \n -> if (n == 0) then true else not (even (n - 1))
    in even 1
  */
  uint8_t code [] =
    { 254,237,202,254,1,0,2,0,0,0,56,0,0,0,0,0,0,0,0,0,0,0,52,4,6,0,0,5,3,0,16,0,36,12,14,13,10,0,40,15,4,4,2,0,12,5,6,0,1,32,18,0,58,7,1,17,0,72,4,2,0,12,23,5,3,1,16,0,36,12,14,22,12,15,12 };

  vmc_t container;

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_5\n");
    return false;
  }


  if(container.contexts[0].env.value == 1){ //env register contains True?
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

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_6\n");
    return false;
  }

  if(container.contexts[0].env.value == 0){ //env register contains False?
    return true;
  } else {
    return false;
  }
}

bool vmc_run_7_test(){

  /*
   * Program 6 with the optimised bytecode
  */

  uint8_t code [] =
    { 254,237,202,254,1,0,2,0,0,0,53,0,0,0,0,0,0,0,0,0,0,0,63,49,6,0,0,5,16,0,37,14,13,52,0,71,15,10,0,41,15,4,1,49,6,0,1,32,18,0,54,7,1,15,4,4,1,23,5,0,16,0,37,14,5,0,16,0,33,14,15,3,0,49,7,1,32,53,0,83,7,0,15,7,1,15 };

  vmc_t container;

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_7\n");
    return false;
  }

  if(container.contexts[0].env.value == 0){ //env register contains False?
    return true;
  } else {
    return false;
  }

}


bool vmc_run_8_test(){

  /*
    let foo = \x -> x + 11
    in let r = 2
    in foo r

  */
  uint8_t code [] =
    { 254,237,202,254,1,0,2,0,0,0,2,0,0,0,11,0,0,0,0,0,0,0,22,52,0,37,49,6,0,0,9,4,1,5,0,14,13,3,0,49,6,0,1,24,15 };

  vmc_t container;

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_8\n");
    return false;
  }

  if(container.contexts[0].env.value == 13){
    return true;
  } else {
    return false;
  }
}

bool vmc_run_9_test(){

  /*
    chan : Channel Int
    chan = channel ()

    process1 : () -> ()
    process1 void = sync (send chan 5)

    main =
    let _ = spawn process1 in
    let v = sync (recv chan) in
    v

  */

  uint8_t code [] = { 254,237,202,254,1,0,1,0,0,0,5,0,0,0,0,0,0,0,34,8,55,1,4,10,0,39,9,4,1,55,0,9,3,2,55,3,55,4,13,4,1,9,3,2,49,6,0,0,55,2,55,4,15,13 };

  vmc_t container;

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_9\n");
    return false;
  }

  if(container.contexts[0].env.value == 5 && // receiver
     container.contexts[1].env.value == 0){  // sender
    return true;
  } else {
    return false;
  }
}


bool vmc_run_10_test(){

  /*

    chan : Channel Int
    chan = channel ()

    plus3 : Int -> Int
    plus3 x = x + 3

    process1 : () -> ()
    process1 void = sync (send chan 5)

    main =
    let _ = spawn process1 in
    let v = sync (wrap (recv chan) plus3) in
    v


  */

  uint8_t code [] ={ 254,237,202,254,1,0,2,0,0,0,3,0,0,0,5,0,0,0,0,0,0,0,54,8,55,1,49,52,0,54,9,4,10,0,62,9,4,1,55,0,9,4,3,3,55,3,5,2,2,55,7,55,4,13,3,0,49,6,0,0,24,15,4,1,9,3,3,49,6,0,1,55,2,55,4,15,13 };

  vmc_t container;

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_10\n");
    return false;
  }

  if(container.contexts[0].env.value == 8 && // receiver
     container.contexts[1].env.value == 0){  // sender
    return true;
  } else {
    return false;
  }
}

bool vmc_run_11_test(){

  /*

    chan : Channel Int
    chan = channel ()

    process1 : () -> ()
    process1 void = sync (send chan 5)

    main =
    let _ = spawn process1 in
    let m = 7 in
    let v = sync (wrap (recv chan) (\x -> x + m)) in
    v


  */

  uint8_t code [] = {254,237,202,254,1,0,2,0,0,0,7,0,0,0,5,0,0,0,0,0,0,0,66,8,55,1,4,10,0,64,9,49,52,0,84,9,4,2,1,55,0,9,49,6,0,0,9,4,3,4,55,3,5,4,1,5,2,2,14,55,7,55,4,13,4,1,9,3,2,49,6,0,1,55,2,55,4,15,4,1,5,0,24,15,10,0,78,15,13};


  vmc_t container;

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_11\n");
    return false;
  }

  if(container.contexts[0].env.value == 12 && // receiver
     container.contexts[1].env.value == 0){   // sender
    return true;
  } else {
    return false;
  }
}

/*FFI TEST*/
bool vmc_run_12_test(){

  /*

    foreign import add : Int -> Int -> Int

    sub : Int -> Int -> Int
    sub x y = x - y

    main = add 22 35

  */
  uint8_t code [] =
    {254,237,202,254,1,0,2,0,0,0,35,0,0,0,22,0,0,0,1,0,0,2,0,0,0,0,25,52,0,42,6,0,0,49,6,0,1,56,0,0,13,4,0,5,1,26,15,10,0,36,15,13};


  vmc_t container;

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_12\n");
    return false;
  }

  if(container.contexts[0].env.value == 57){
    return true;
  } else {
    return false;
  }
}

bool vmc_run_13_test(){

  /*


    foreign import prnLst : List -> ()
    foreign import allocLst : () -> List

    data List where
         Nil  : List
         Cons : Int -> List -> List

    foo = Cons 5 (Cons 3 Nil)

    main = let list = allocLst () in
           prnLst list


  */
  uint8_t code [] =
    //{254,237,202,254,1,0,2,0,0,0,3,0,0,0,5,0,0,0,1,0,0,1,0,0,0,0,43,52,0,65,49,8,11,0,1,9,4,4,1,5,49,6,0,0,5,0,14,14,5,49,6,0,1,5,0,14,14,56,0,0,13,11,0,0,15,10,0,61,15,13};
    {254,237,202,254,1,0,2,0,0,0,3,0,0,0,5,0,0,0,2,0,0,1,0,0,1,1,0,0,0,0,47,52,0,73,49,8,11,0,1,9,4,4,1,5,49,6,0,0,5,0,14,14,5,49,6,0,1,5,0,14,14,8,56,0,1,56,0,0,13,11,0,0,15,10,0,69,15,13};


  vmc_t container;

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_13\n");
    return false;
  }

  if(container.contexts[0].env.value == 0){
    return true;
  } else {
    return false;
  }
}

bool vmc_run_14_test(){

  /*


    foreign import prnLst : List -> ()
    foreign import allocLst : () -> List

    data List where
         Nil  : List
         Cons : Int -> List -> List

    foo = Cons 6 (Cons 8 Nil)

    append : List -> List -> List
    append xs ys =
        case xs of
          Nil -> ys
          Cons x xss -> append xss ys

    main = let list = allocLst () in
           prnLst (append list foo)



  */
  uint8_t code [] =
//    {254,237,202,254,1,0,2,0,0,0,8,0,0,0,6,0,0,0,2,0,0,1,0,0,1,1,0,0,0,0,125,52,0,90,49,8,11,0,1,9,4,4,4,1,5,49,6,0,0,5,0,14,14,5,49,6,0,1,5,0,14,14,9,49,8,56,0,1,9,4,2,1,5,4,1,5,0,51,51,16,0,94,56,0,0,13,11,0,0,15,10,0,86,15,4,4,2,1,5,1,9,9,4,1,0,19,2,0,1,0,115,0,0,0,119,2,1,1,15,4,4,2,1,1,5,4,1,1,5,3,4,51,51,16,0,94,5,4,1,0,5,3,6,14,14,15,15,10,0,94,15,10,0,147,15,13};
//    {254,237,202,254,1,0,2,0,0,0,8,0,0,0,6,0,0,0,2,0,0,1,0,0,1,1,0,0,0,0,47,52,0,73,49,8,11,0,1,9,4,4,1,5,49,6,0,0,5,0,14,14,5,49,6,0,1,5,0,14,14,8,56,0,1,56,0,0,13,11,0,0,15,10,0,69,15,13};

    {



      254,237,202,254,1,0,4,0,0,4,149,0,0,0,3,0,0,0,0,0,0,0,1,0,0,0,6,0,0,1,0,0,1,1,0,0,2,1,0,0,3,2,0,0,4,1,0,0,5,1,0,0,0,2,81,52,0,105,49,8,11,0,1,9,49,52,0,113,9,49,8,11,0,1,9,4,10,0,117,9,10,1,32,49,6,0,0,5,49,6,0,1,5,49,7,0,5,14,14,14,13,11,0,0,15,10,0,101,15,11,0,0,15,10,0,109,15,4,1,9,4,2,2,5,4,1,5,3,2,51,51,17,1,161,4,4,2,2,5,4,2,1,5,1,9,9,9,4,1,1,1,56,0,5,9,4,4,4,2,1,1,1,5,2,5,14,5,4,2,1,1,0,5,3,5,51,51,16,1,36,9,49,8,56,0,4,9,4,4,2,1,5,3,8,51,16,1,222,9,49,8,56,0,2,9,4,4,2,5,0,18,0,221,2,1,56,0,1,17,0,222,8,9,49,8,56,0,0,9,4,2,7,1,1,56,0,5,9,49,8,56,0,4,9,4,4,2,7,5,2,9,1,1,56,0,3,9,49,8,56,0,2,9,4,2,11,0,18,1,22,2,1,56,0,1,17,1,23,8,8,56,0,0,15,10,0,134,15,10,1,28,15,4,4,2,1,5,1,9,9,4,1,0,49,6,0,2,32,18,1,58,2,4,15,4,4,1,1,5,4,1,0,49,6,0,3,26,5,3,3,51,51,16,1,36,5,4,1,1,5,3,3,51,51,17,1,100,15,10,1,36,15,10,1,92,15,4,4,2,1,5,1,9,9,4,1,0,19,2,0,1,1,121,0,0,1,125,2,1,1,15,4,4,2,1,1,5,4,1,1,5,3,4,51,51,16,1,100,5,4,1,0,5,2,6,14,14,15,15,10,1,100,15,10,1,153,15,4,4,2,1,5,1,9,9,4,1,0,49,6,0,2,32,18,1,183,1,1,15,4,4,1,1,5,4,1,0,5,2,4,14,14,5,4,1,0,49,6,0,3,26,5,3,3,51,51,17,1,161,15,10,1,161,15,10,1,214,15,4,1,9,4,1,19,2,0,1,1,237,0,0,1,240,2,5,15,4,4,1,1,5,3,3,51,16,1,222,5,4,1,0,5,3,3,51,51,17,2,12,15,10,1,222,15,4,4,2,1,5,1,9,9,4,1,1,19,2,0,1,2,33,0,0,2,52,4,2,6,5,4,2,1,0,49,6,0,3,9,5,3,7,14,14,15,4,4,2,1,0,5,1,3,2,32,18,2,92,4,1,1,5,4,4,2,1,0,5,1,2,1,49,6,0,3,24,9,5,3,7,14,14,17,2,125,4,4,1,1,5,4,2,1,0,5,3,4,51,51,16,2,12,5,4,4,1,3,2,5,1,2,1,9,5,3,7,14,14,15,15,10,2,12,15,10,2,127,15,13


    };

  vmc_t container;

  int i = setup_and_run(&container, code, sizeof(code));
  if(i == -1){
    printf("Failure in vmc_run_14\n");
    return false;
  }

  if(container.contexts[0].env.value == 0){
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

  // SEE NOTE 1 to find out config changes needed to
  // make program 5 and 6 run
  /* bool t5 = vmc_run_5_test(); */
  /* test_stat("vmc_run_5", &total, t5); */
  /* bool t6 = vmc_run_6_test(); */
  /* test_stat("vmc_run_6", &total, t6); */
  /* bool t7 = vmc_run_7_test(); */
  /* test_stat("vmc_run_7", &total, t7); */

  bool t8 = vmc_run_8_test();
  test_stat("vmc_run_8", &total, t8);
  bool t9 = vmc_run_9_test();
  test_stat("vmc_run_9", &total, t9);
  bool t10 = vmc_run_10_test();
  test_stat("vmc_run_10", &total, t10);
  bool t11 = vmc_run_11_test();
  test_stat("vmc_run_11", &total, t11);
  /* bool t12 = vmc_run_12_test(); */
  /* test_stat("vmc_run_12", &total, t12); */
  /* bool t13 = vmc_run_13_test(); */
  /* test_stat("vmc_run_13", &total, t13); */
  bool t14 = vmc_run_14_test();
  test_stat("vmc_run_14", &total, t14);

  if (t1 && t2 && t3 && t4 && t8 && t9 && t10 && t11 && t14) {
    printf("Passed total : %d/%d tests\n", total, 9);
    return 1;
  }
  return -1;
}
