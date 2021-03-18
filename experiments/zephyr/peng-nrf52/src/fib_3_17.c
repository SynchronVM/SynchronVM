#include "peng-platform.h"
#include "peng.h"
#include <stdio.h>

typedef struct {
    /* Generic procedure fields */
    ACTIVATION_RECORD_FIELDS;
    /* Procedure specific fields */
    sv_int_t *r; // Procedure argument
    trigger_t trig1;
} act_mywait_t;

act_mywait_t *enter_mywait(act_t *caller, uint32_t priority, uint8_t depth, sv_int_t *r);
void step_mywait(act_t *gen_act);

act_mywait_t *enter_mywait(act_t *caller, uint32_t priority, uint8_t depth, sv_int_t *r) {
    act_mywait_t *act = (act_mywait_t *) enter(sizeof(act_mywait_t), step_mywait, caller, priority, depth);
    act->r = r;
    act->trig1.act = (act_t *) act;
    return act;
};

void step_mywait(act_t *gen_act) {
    act_mywait_t *act = (act_mywait_t *) gen_act;
    switch(act->pc) {
        case 0:
            sensitize((sv_t *) act->r, &act->trig1);
            act->pc = 1;
            return;

        case 1:

        leave((act_t *) act, sizeof(act_mywait_t));
    }
}

typedef struct {
    /* Generic procedure fields */
    ACTIVATION_RECORD_FIELDS;
    /* Procedure specific fields */
    sv_int_t *r1; // Procedure argument
    sv_int_t *r2; // Procedure argument
    sv_int_t *r; // Procedure argument
    sv_int_t v1; // Declared at (16, 5) in file Fib.hs
    sv_int_t v2; // Declared at (17, 5) in file Fib.hs
} act_mysum_t;

act_mysum_t *enter_mysum(act_t *caller, uint32_t priority, uint8_t depth, sv_int_t *r1, sv_int_t *r2, sv_int_t *r);
void step_mysum(act_t *gen_act);

act_mysum_t *enter_mysum(act_t *caller, uint32_t priority, uint8_t depth, sv_int_t *r1, sv_int_t *r2, sv_int_t *r) {
    act_mysum_t *act = (act_mysum_t *) enter(sizeof(act_mysum_t), step_mysum, caller, priority, depth);
    act->r1 = r1;
    act->r2 = r2;
    act->r = r;
    return act;
};

void step_mysum(act_t *gen_act) {
    act_mysum_t *act = (act_mysum_t *) gen_act;
    switch(act->pc) {
        case 0:
            {
            uint8_t new_depth = act->depth - 1;
            uint32_t pinc = 1 << new_depth;
            uint32_t new_priority = act->priority;
            fork_routine((act_t *) enter_mywait( (act_t *) act
                                               , new_priority
                                               , new_depth
                                               , act->r1));
            new_priority += pinc;
            fork_routine((act_t *) enter_mywait( (act_t *) act
                                               , new_priority
                                               , new_depth
                                               , act->r2));
            }
            act->pc = 1;
            return;

        case 1:
            assign_int(&act->v1, act->priority, act->r1->value);
            assign_int(&act->v2, act->priority, act->r2->value);
            later_int(act->r, now + 1, (act->v1.value) + (act->v2.value));

        leave((act_t *) act, sizeof(act_mysum_t));
    }
}

typedef struct {
    /* Generic procedure fields */
    ACTIVATION_RECORD_FIELDS;
    /* Procedure specific fields */
    sv_int_t n; // Procedure argument
    sv_int_t *r; // Procedure argument
    sv_int_t r1; // Declared at (22, 5) in file Fib.hs
    sv_int_t r2; // Declared at (23, 5) in file Fib.hs
} act_myfib_t;

act_myfib_t *enter_myfib(act_t *caller, uint32_t priority, uint8_t depth, int n, sv_int_t *r);
void step_myfib(act_t *gen_act);

act_myfib_t *enter_myfib(act_t *caller, uint32_t priority, uint8_t depth, int n, sv_int_t *r) {
    act_myfib_t *act = (act_myfib_t *) enter(sizeof(act_myfib_t), step_myfib, caller, priority, depth);
    initialize_int(&act->n);
    assign_int(&act->n, act->priority, n);
    act->r = r;
    return act;
};

void step_myfib(act_t *gen_act) {
    act_myfib_t *act = (act_myfib_t *) gen_act;
    switch(act->pc) {
        case 0:
            initialize_int(&act->r1);
            assign_int(&act->r1, act->priority, 0);
            initialize_int(&act->r2);
            assign_int(&act->r2, act->priority, 0);
            if (!((act->n.value) < (2))) goto L0;
            later_int(act->r, now + 1000, 1);
            goto L1;

        L0:
            {
            uint8_t new_depth = act->depth - 1;
            uint32_t pinc = 1 << new_depth;
            uint32_t new_priority = act->priority;
            fork_routine((act_t *) enter_myfib( (act_t *) act
                                              , new_priority
                                              , new_depth
                                              , (act->n.value) - (1)
                                              , &act->r1));
            new_priority += pinc;
            fork_routine((act_t *) enter_myfib( (act_t *) act
                                              , new_priority
                                              , new_depth
                                              , (act->n.value) - (2)
                                              , &act->r2));
            new_priority += pinc;
            fork_routine((act_t *) enter_mysum( (act_t *) act
                                              , new_priority
                                              , new_depth
                                              , &act->r1
                                              , &act->r2
                                              , act->r));
            }
            act->pc = 1;
            return;

        case 1:

        L1:

        leave((act_t *) act, sizeof(act_myfib_t));
    }
}

