/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Joel Svensson, Abhiroop Sarkar 				  */
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

#include <powerman.h>

#include <usb_cdc.h>
#include <power/power.h>
#include <kernel.h>
#include <ksched.h>


char *power_states[] = { "PM_STATE_ACTIVE",
			 "PM_STATE_RUNTIME_IDLE",
			 "PM_STATE_SUSPEND_TO_IDLE",
			 "PM_STATE_STANDBY",
			 "PM_STATE_SUSPEND_TO_RAM",
			 "PM_STATE_SUSPEND_TO_DISK",
			 "PM_STATE_SOFT_OFF" };

volatile static enum pm_state curr_state = PM_STATE_ACTIVE;

struct pm_notifier power_state_change;

static void powerman_state_entry(enum pm_state s) {

  
}

static void powerman_state_exit(enum pm_state s) {

  
  
}

/* Should be possible to set pm_power_state_exit_post_ops 
   to replace the built in implementation. 
   I dont seem to have succeeded */ 
__weak void pm_power_state_exit_post_ops(struct pm_state_info info) {
  /* pm_system_suspend is entered with irq locked
   * unlock irq before leave pm_system_suspend
   */
  irq_unlock(0);
  /*printk("powerman: exit post ops actually runs...\r\n");*/
}

__weak void pm_power_state_set(struct pm_state_info info) {
  printk("powerman: set power state\r\n");
}

struct pm_state_info pm_policy_next_state(int ticks)
{
  struct pm_state_info info = {};

  //zassert_true(z_is_idle_thread_object(_current), NULL);
  //zassert_true(ticks == _kernel.idle, NULL);
  //idle_entered = true;
  if (!z_is_idle_thread_object(_current)) {
    printk("Not called from idle thread\r\n");
    info.state = curr_state;
    return info;
  }
  
  //printk("ticks: %d\r\n", ticks);
  //zassert_true(ticks == _kernel.idle, NULL);
  if (ticks > 20000) {
    //info.state = PM_STATE_STANDBY;
    //info.state = PM_STATE_RUNTIME_IDLE;
    info.state = PM_STATE_SOFT_OFF;
    //info.state = PM_STATE_ACTIVE; // always stay active as a test
  } else {
    info.state = PM_STATE_ACTIVE;
  }
  
  return info;
}


bool powerman_init(void) {

  power_state_change.state_entry = powerman_state_entry;
  power_state_change.state_exit  = powerman_state_exit;

  pm_notifier_register(&power_state_change);
		      
  return true; 
}

  

  
