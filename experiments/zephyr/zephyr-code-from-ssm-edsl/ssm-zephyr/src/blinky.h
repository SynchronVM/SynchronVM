#ifndef BLINKY_H_
#define BLINKY_H_

struct act_main {
   ACTIVATION_RECORD_FIELDS;
  sv_int_t *led;
  trigger_t trigger1;
};

typedef struct act_main act_main_t;

extern act_main_t *enter_main( act_t *parent
			       , priority_t priority
			       , depth_t depth
			       , sv_int_t *led );


extern void init_blinky(void);
#endif
