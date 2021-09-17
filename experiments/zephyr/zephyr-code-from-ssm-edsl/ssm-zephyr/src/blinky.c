#include <peng.h>
#include <peng-platform.h>

#include "led.h"

struct act_main {
   ACTIVATION_RECORD_FIELDS;
  sv_int_t *led;
  trigger_t trigger1;
};

typedef struct act_main act_main_t;

led_driver_t *led;

void init_blinky(void){
  led = led_init(1);
}


void step_main(act_t *bare_act)
{
  act_main_t *act = (act_main_t *) bare_act;
  switch (act->pc)
  {

  case 0: ;

  L0: ;
    led_set(led, act->led->value);
    later_int(act->led, now + 5000000L, 1);
    sensitize((sv_t *) act->led, &act->trigger1);
    act->pc = 1;
    return;

  case 1: ;
    desensitize(&act->trigger1);
    led_set(led, act->led->value);
    later_int(act->led, now + 5000000L, 0);
    sensitize((sv_t *) act->led, &act->trigger1);
    act->pc = 2;
    return;

  case 2: ;
    desensitize(&act->trigger1);
    goto L0;
  }
  leave((act_t *) act, sizeof(act_main_t));
}

act_main_t *enter_main( act_t *parent
                      , priority_t priority
                      , depth_t depth
                      , sv_int_t *led )
{
  act_main_t *act = (act_main_t *) enter( sizeof(act_main_t)
                                        , step_main
                                        , parent
                                        , priority
                                        , depth );
  act->led = led;
  act->trigger1.act = (act_t *) act;
  return act;
}

