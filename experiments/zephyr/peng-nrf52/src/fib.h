#ifndef FIB_H_
#define FIB_H_

#include "peng-platform.h"
#include "peng.h"

typedef struct {
    /* Generic procedure fields */
    ACTIVATION_RECORD_FIELDS;
    /* Procedure specific fields */
    sv_int_t n; // Procedure argument
    sv_int_t *r; // Procedure argument
    sv_int_t r1; // Declared at (22, 5) in file Fib.hs
    sv_int_t r2; // Declared at (23, 5) in file Fib.hs
} act_myfib_t;


extern act_myfib_t *enter_myfib(act_t *caller, uint32_t priority, uint8_t depth, int n, sv_int_t *r);


#endif

