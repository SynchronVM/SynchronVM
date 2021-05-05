/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Joel Svensson, Abhiroop Sarkar             		  */
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

/*******************/
/* Zephyr includes */
#include <zephyr/types.h>
#include <zephyr.h>
#include <sys/printk.h>
#include <sys/byteorder.h>
#include <sys/ring_buffer.h>

/*********************/
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>

/********************/
/* SenseVM Includes */

#include <vm-conf.h>
#include <VMC.h>
#include <ll_driver.h>

/***************************************************/
/* Check for configurations that are not sensible. */
/* Compile time error stuff...                     */

#if VMC_NUM_CONTAINERS > 4
#error "Too many containers specified in vm-conf.h"
#endif

/*********************************************/
/* Declare stacks and threads for containers */

#define STACK_SIZE 512

struct k_thread vmc_zephyr_thread[4];
k_thread_stack_t *vmc_zephyr_stack[4];
struct k_mbox zephyr_thread_mbox[4];

vmc_t vm_containers[4]; /* SenseVM containers */
static const int   vm_id[4] = {0,1,2,3};

const char* container_names[4] = { "C0", "C1", "C2", "C3" };


#if VMC_NUM_CONTAINERS >= 1
K_THREAD_STACK_DEFINE(vmc_zephyr_stack_0, STACK_SIZE);
#else
k_thread_stack_t *vmc_zephyr_stack_0 = NULL;
#endif
#if VMC_NUM_CONTAINERS >= 2
K_THREAD_STACK_DEFINE(vmc_zephyr_stack_1, STACK_SIZE);
#else
k_thread_stack_t *vmc_zephyr_stack_1 = NULL;
#endif
#if VMC_NUM_CONTAINERS >= 3
K_THREAD_STACK_DEFINE(vmc_zephyr_stack_2, STACK_SIZE);
#else
k_thread_stack_t *vmc_zephyr_stack_2 = NULL;
#endif
#if VMC_NUM_CONTAINERS >= 4
K_THREAD_STACK_DEFINE(vmc_zephyr_stack_3, STACK_SIZE);
#else
k_thread_stack_t *vmc_zephyr_stack_3 = NULL;
#endif

/***********************************************/
/*  Thoughts                                   */
/*
 *  The nrf52 boards run at 80MHz. 80 000 000 cycles per second
 *   - IPC is definitely over 1.
 *   - 80000 clock cycles per ms. < 80000 instructions per ms
 *   - How many arm instructions per CAM instruction ?
 *   - What would be a good time length to run the CAM interpreter
 *     between returns to the container thread.
 *
 *  The stm32f407 runs at 168MHz. 168 000 000 cycles per second
 *   - 168000 clock cycles per ms.
 *
 *
 *  * How much time does it take to poll the mbox?
 *  * How much time does it take to affix work onto the work queue?
 *  * If we have more than one container, how often does it make
 *    sense to allow for a container "switch".
 *
 *  Thread Priority
 *  * Negative thread priority is non-preemtable thread. (Cooperative threads)
 *    Positive are preemtable.
 *
 *
 */

//int k_mbox_put(struct k_mbox *mbox, struct k_mbox_msg *tx_msg, k_timeout_t timeout)

// int zephyr_send_message(


/***********************************************/
/* Zephyr thread for containing a VM container */

void zephyr_container_thread(void* vmc, void* vm_id, void* c) {
  (void)c;  /* These are unused so far. otherwise a way to pass arguments to the thread */ 

  vmc_t *container = vmc;
  int id = *(int*)vm_id;

  struct k_mbox_msg recv_msg;

  while (1) {

    /* Do stuff */
    /* Like run the scheduler */

    /*scheduler(datastructure of info on what happened); */

    if (k_mbox_get(&zephyr_thread_mbox[id], &recv_msg, NULL, K_NO_WAIT) == 0) {
      /* There was a message */

      /* Maybe loop here to receive all messages */

      /* enqueue on shared datastructure with scheduler */

    } else {
      /* block until there is a message */

      k_mbox_get(&zephyr_thread_mbox[id], &recv_msg, NULL, K_FOREVER);
    }

    /* use the messages from the mbox to add tasts to the
       queue for the next launch of the scheduler */

    /* There should be an interface in VMC.h for
       creating this datastructure to pass data
       back and forth from the zephyr thread and the
       scheduler in the container */

    /* This thread may need to yield to allow other threads
       (containers) to run.
       If blocking on the mailbox this yield will happen automativally.
       In other cases it may need some coercion ;)
    */

  }
}

bool zephyr_start_container_threads(void) {

  bool r = true;

  for (int i = 0; i < VMC_NUM_CONTAINERS; i ++) {

    /* We can set different priorities on
       different containers */

    k_tid_t t = k_thread_create(&vmc_zephyr_thread[i], vmc_zephyr_stack[i],
				K_THREAD_STACK_SIZEOF(vmc_zephyr_stack[i]),
				zephyr_container_thread,
				(void*)&(vm_containers[i]), (void*)&vm_id[i], NULL,
				5, 0, K_NO_WAIT);
    if (t) {
      k_thread_name_set(t, container_names[i]);
    }

    if (!t) r = false;
  }

  return r;
}

bool zephyr_sensevm_init(void) {

  bool r = false;

  /* intialize the vm_containers. max 4 of them */
  if (vmc_init(vm_containers, 4)) {

    vmc_zephyr_stack[0] = vmc_zephyr_stack_0;
    vmc_zephyr_stack[1] = vmc_zephyr_stack_1;
    vmc_zephyr_stack[2] = vmc_zephyr_stack_2;
    vmc_zephyr_stack[3] = vmc_zephyr_stack_3;

    /* Initialize all message boxes */

    for (int i = 0; i < VMC_NUM_CONTAINERS; i ++) {
      k_mbox_init(&zephyr_thread_mbox[i]);
    }
    r = true;
  }

  return r;
}
