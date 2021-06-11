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


/********************/
/* Chibios includes */
#include "ch.h"
#include "hal.h"

/*********************/
/* stdlib includes   */
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>


/********************/
/* SenseVM Includes */

#include <vm-conf.h>
#include <VMC.h>
#include <scheduler.h>
#include <ll/ll_driver.h>

#include <hal/chibios/svm_chibios.h>

/***************************************************/
/* Check for configurations that are not sensible. */
/* Compile time error stuff...                     */

#if VMC_NUM_CONTAINERS > 4
#error "Too many containers specified in vm-conf.h"
#endif

/********************************************************/
/* Declare stacks, threads and mailboxes for containers */

#define STACK_SIZE  1024
#define MAX_MESSAGES 100

static mailbox_t mb[VMC_NUM_CONTAINERS];
static msg_t b[VMC_NUM_CONTAINERS][MAX_MESSAGES];

// A chibios message is large enough to hold a pointer.
// So ll_driver_msg_t struct has to be stored elsewhere.
// TODO: Come up with solution.

static THD_WORKING_AREA(thread_wa[VMC_NUM_CONTAINERS],
                        STACK_SIZE);

static thread_t *threads[VMC_NUM_CONTAINERS];

vmc_t vm_containers[4];
const char* container_names[4] = { "C0", "C1", "C2", "C3" };

#define CONTAINER_PRIORITY  (tprio_t)(NORMALPRIO-20)

/*************************/
/* Container thread data */

typedef struct {
  vmc_t* container;
  const char* container_name;
} chibios_svm_thread_data_t;

chibios_svm_thread_data_t thread_data[VMC_NUM_CONTAINERS];


/****************************/
/* Chibios container thread */

static THD_FUNCTION(chibios_container_thread, arg) {

  chibios_svm_thread_data_t *data = (chibios_svm_thread_data_t*)arg;
  vmc_t * container = data->container;

  chRegSetThreadName(data->container_name);

  // TODO: vmc_run
  // TODO: Call scheduler

  
}


bool chibios_start_container_threads(void) { 

  bool r = true;

  for (int i = 0; i < VMC_NUM_CONTAINERS; i++) {
    chMBObjectInit(&mb[i], b[i], MAX_MESSAGES);
    
    thread_data[i].container = &vm_containers[i];
    thread_data[i].container_name = container_names[i];
    
    threads[i] = chThdCreateStatic(thread_wa[i],
				   sizeof thread_wa[i],
				   CONTAINER_PRIORITY,
				   chibios_container_thread,
				   (void *)&thread_data[i]);
  }
  
}


bool chibios_sensevm_init(void) {

  
  
  
}
