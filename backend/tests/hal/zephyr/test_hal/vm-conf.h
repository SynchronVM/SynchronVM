/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Joel Svensson, Abhiroop Sarkar       			  */
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

#ifndef __VM_CONF_H_
#define __VM_CONF_H_

/***********************************************/
/* Sense-VM Application Configuration Settings */
/***********************************************/


/********************************************/
/* Virtual Machine Container (VMC) settings */
/********************************************/

#define VMC_NUM_CONTAINERS 1 // Defines the number of Virtual Machine Containers to run


/*********/
/* VMC 1 */
/*********/
#define VMC_CONTAINER_1_HEAP_SIZE_BYTES       8192
#define VMC_CONTAINER_1_BYTECODE_FILE         "test.X"
#define VMC_CONTAINER_1_STACK_SIZE_BYTES      1024
#define VMC_CONTAINER_1_ARRAY_MEM_SIZE_BYTES  4096

#define VMC_CONTAINER_1_USE_UART_0            1    // Correspond to Aliases in DTS overlay
#define VMC_CONTAINER_1_USE_BUTTON_0          1

/*
  Each channel has a max capacity of storing 3 elements
  in sendq and 3 elements in recvq.x
  sendq = 6 bytes * 3 = 18 bytes
  recvq = 2 byte  * 3 =  6 bytes
  Total -> 24 bytes
  Assuming 100 channels -> 2400 bytes
  MAX_CHANNELS and MAX_WAIT_PARTICIPANTS can move here
*/

#define VMC_CONTAINER_1_CHANNEL_MEM_SIZE_BYTES  2400

/*********/
/* VMC 2 */
/*********/
//#define VMC_CONTAINER_2_HEAP_SIZE_BYTES      8192
//#define VMC_CONTAINER_2_BYTECODE_FILE        "tests/test.X"
//#define VMC_CONTAINER_2_STACK_SIZE_BYTES     1024
//#define VMC_CONTAINER_2_ARRAY_MEM_SIZE_BYTES  4096

//#define VMC_CONTAINER_2_USE_UART_0            1
//#define VMC_CONTAINER_2_USE_UART_1            1


/********************************/
/* Configuration error checking */
/********************************/
/* TODO: Autogenerate these rules */ 

#if defined VMC_CONTAINER_1_USE_UART_0 && \
    defined VMC_CONTAINER_2_USE_UART_0  
#error Resource UART_0 shared between containers 
#endif

#if defined VMC_CONTAINER_1_USE_UART_1 && \
    defined VMC_CONTAINER_2_USE_UART_1  
#error Resource UART_1 shared between containers 
#endif



#endif
