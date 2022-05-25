/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2022 Abhiroop Sarkar                            		  */
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

#ifndef __TRUSTEDRTS_H_
#define __TRUSTEDRTS_H_

#include <trusted/TrustedVMC.h>
#include <event.h>
#include <sys/sys_time.h>

extern int channel(vmc_trusted_t *container, UUID    *chan_id);

extern int dispatch(vmc_trusted_t *container);

extern int spawn  (vmc_trusted_t *container, uint16_t label  );

extern int sync   (  vmc_trusted_t       *container, event_t   *evts);

extern int sendEvt(  vmc_trusted_t       *container
                   , UUID        *chan_id
                   , cam_value_t  msg
                   , event_t     *sevt);

extern int recvEvt(  vmc_trusted_t   *container
                   , UUID    *chan_id
                   , event_t *revt);

extern int choose (  vmc_trusted_t   *container
                   , event_t *evt1
                   , event_t *evt2
                   , event_t *evts);

extern int time (vmc_trusted_t *container, Time baseline, Time deadline);

extern int handle_msg(vmc_trusted_t *vmc, svm_msg_t *m);

#endif
