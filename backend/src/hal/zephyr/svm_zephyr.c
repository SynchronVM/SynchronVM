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
#include <zephyr.h>

/*********************/
/* stdlib includes   */
#include <stdbool.h>

/********************/
/* SenseVM Includes */

#include <vm-conf.h>
#include <VMC.h>
#include <scheduler.h>
#include <ll/ll_driver.h>

#include <hal/zephyr/svm_zephyr.h>

/***************************************************/
/* Check for configurations that are not sensible. */
/* Compile time error stuff...                     */

#if VMC_NUM_CONTAINERS > 4
#error "Too many containers specified in vm-conf.h"
#endif

#if VMC_NUM_CONTAINERS <= 0
#error "Too few containers specified in vm-conf.h"
#endif

/*********************************************/
/* Declare stacks and threads for containers */

#define STACK_SIZE  1024
#define MAX_MESSAGES 100
#define MSG_ALIGNMENT 4

struct k_thread vmc_zephyr_thread[VMC_NUM_CONTAINERS];
k_thread_stack_t *vmc_zephyr_stack[VMC_NUM_CONTAINERS];

vmc_t vm_containers[VMC_NUM_CONTAINERS]; /* SenseVM containers */

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

zephyr_interop_t zephyr_interop[4];


/******************/
/* Message Queues */

#if VMC_NUM_CONTAINERS >= 1
K_MSGQ_DEFINE(message_queue_0, sizeof(ll_driver_msg_t),MAX_MESSAGES, MSG_ALIGNMENT);
#endif
#if VMC_NUM_CONTAINERS >= 2
K_MSGQ_DEFINE(message_queue_1, sizeof(ll_driver_msg_t),MAX_MESSAGES, MSG_ALIGNMENT);
#endif
#if VMC_NUM_CONTAINERS >= 3
K_MSGQ_DEFINE(message_queue_2, sizeof(ll_driver_msg_t),MAX_MESSAGES, MSG_ALIGNMENT);
#endif
#if VMC_NUM_CONTAINERS >= 4
K_MSGQ_DEFINE(message_queue_3, sizeof(ll_driver_msg_t),MAX_MESSAGES, MSG_ALIGNMENT);
#endif

struct k_msgq *message_queues[VMC_NUM_CONTAINERS];

/*******************************/
/* Send_message implementation */


int send_message(zephyr_interop_t* this, ll_driver_msg_t msg) {

  return k_msgq_put(this->msgq,(void*)&msg, K_NO_WAIT);
}

int read_message_poll(vmc_t *vmc, ll_driver_msg_t *msg) {
  zephyr_interop_t* interop = (zephyr_interop_t*)vmc->backend_custom;

  int r = k_msgq_get(interop->msgq, (void*)msg, K_NO_WAIT);

  if (r == -ENOMSG || r == -EAGAIN) {
    return VMC_NO_MESSAGE;
  }
  return VMC_MESSAGE_RECEIVED;
}

int read_message_block(vmc_t *vmc, ll_driver_msg_t *msg) {
  zephyr_interop_t* interop = (zephyr_interop_t*)vmc->backend_custom;

  int r = k_msgq_get(interop->msgq, (void*)msg, K_FOREVER);

  if (r == -ENOMSG || r == -EAGAIN) {
    return VMC_NO_MESSAGE;
  }
  return VMC_MESSAGE_RECEIVED;
}

uint32_t message_queue_num_used(vmc_t *vmc) {
  zephyr_interop_t* interop = (zephyr_interop_t*)vmc->backend_custom;

  return k_msgq_num_used_get(interop->msgq);
}

/***********************************************/
/*  Thoughts on threads                        */
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

/***********************************************/
/* Zephyr thread for containing a VM container */

/* Maybe this only runs Scheduler. */
void zephyr_container_thread(void* vmc, void* b, void* c) {
  (void)c;  /* These are unused so far. otherwise a way to pass arguments to the thread */ 
  (void)b;
  
  vmc_t *container = (vmc_t *)vmc;

  printk("container address: %u\r\n", (uint32_t)container);

  if (vmc_run(container, printk) != 1) {
    /* error state */
    /* cannot currently happen */
    /* report this to some error handling system */
    return;
  }

  scheduler(container, read_message_poll, read_message_block, message_queue_num_used, printk);

  /* If we return to this point, do something with the 
     return value of scheduler*/
}

/* Must intialize before starting threads.
   run: zephyr_sensevm_init()
*/
bool zephyr_start_container_threads(void) {

  bool r = true;

  for (int i = 0; i < VMC_NUM_CONTAINERS; i ++) {

    /* We can set different priorities on
       different containers */

    k_tid_t t = k_thread_create(&vmc_zephyr_thread[i], vmc_zephyr_stack[i],
    				STACK_SIZE,
    				zephyr_container_thread,
    				(void*)&(vm_containers[i]), NULL, NULL,
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

  /* Stacks are null if not initialized */
#if VMC_NUM_CONTAINERS >= 1
  vmc_zephyr_stack[0] = vmc_zephyr_stack_0;
  message_queues[0] = &message_queue_0;
#endif
#if VMC_NUM_CONTAINERS >= 2
  vmc_zephyr_stack[1] = vmc_zephyr_stack_1;
  message_queues[1] = &message_queue_1;
#endif
#if VMC_NUM_CONTAINERS >= 3
  vmc_zephyr_stack[2] = vmc_zephyr_stack_2;
  message_queues[2] = &message_queue_2;
#endif
#if VMC_NUM_CONTAINERS >= 4
  vmc_zephyr_stack[3] = vmc_zephyr_stack_3;
  message_queues[3] = &message_queue_3;
#endif

  for (int i = 0; i < VMC_NUM_CONTAINERS; i ++) {

    /* Initialize interop functionality */
    //zephyr_interop[i].mbox = &zephyr_thread_mbox[i];
    zephyr_interop[i].msgq = message_queues[i];
    zephyr_interop[i].send_message = send_message;

    /* add zephyr_interop field to vm container */
    vm_containers[i].backend_custom = (void *)&zephyr_interop[i];

  }

  /* Initialize VM containers */
  r = vmc_init(vm_containers, VMC_NUM_CONTAINERS);

  return r;
}
