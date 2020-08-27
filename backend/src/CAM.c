/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Joel Svensson             				  */
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

#include <CAM.h>
#include <VMC.h>

typedef int (*eval_fun) (vmc_t *vmc, uint8_t *bc_rest);

int eval_fst(vmc_t *vmc, uint8_t *bc_rest);
int eval_snd(vmc_t *vmc, uint8_t *bc_rest);
int eval_add(vmc_t *vmc, uint8_t *bc_rest);
int eval_rest(vmc_t *vmc, uint8_t *bc_rest);
int eval_push(vmc_t *vmc, uint8_t *bc_rest);
int eval_swap(vmc_t *vmc, uint8_t *bc_rest);
int eval_loadi(vmc_t *vmc, uint8_t *bc_rest);
int eval_loadb(vmc_t *vmc, uint8_t *bc_rest);
int eval_clear(vmc_t *vmc, uint8_t *bc_rest);
int eval_cons(vmc_t *vmc, uint8_t *bc_rest);
int eval_cur(vmc_t *vmc, uint8_t *bc_rest);
int eval_pack(vmc_t *vmc, uint8_t *bc_rest);
int eval_skip(vmc_t *vmc, uint8_t *bc_rest);
int eval_stop(vmc_t *vmc, uint8_t *bc_rest);
int eval_app(vmc_t *vmc, uint8_t *bc_rest);
int eval_return(vmc_t *vmc, uint8_t *bc_rest);
int eval_call(vmc_t *vmc, uint8_t *bc_rest);
int eval_goto(vmc_t *vmc, uint8_t *bc_rest);
int eval_gotofalse(vmc_t *vmc, uint8_t *bc_rest);
int eval_switch(vmc_t *vmc, uint8_t *bc_rest);
int eval_abs(vmc_t *vmc, uint8_t *bc_rest);
int eval_neg(vmc_t *vmc, uint8_t *bc_rest);
int eval_not(vmc_t *vmc, uint8_t *bc_rest);
int eval_dec(vmc_t *vmc, uint8_t *bc_rest);
int eval_addi(vmc_t *vmc, uint8_t *bc_rest);
int eval_muli(vmc_t *vmc, uint8_t *bc_rest);
int eval_mini(vmc_t *vmc, uint8_t *bc_rest);
int eval_addf(vmc_t *vmc, uint8_t *bc_rest);
int eval_mulf(vmc_t *vmc, uint8_t *bc_rest);
int eval_minf(vmc_t *vmc, uint8_t *bc_rest);
int eval_gt(vmc_t *vmc, uint8_t *bc_rest);
int eval_lt(vmc_t *vmc, uint8_t *bc_rest);
int eval_eq(vmc_t *vmc, uint8_t *bc_rest);
int eval_ge(vmc_t *vmc, uint8_t *bc_rest);
int eval_le(vmc_t *vmc, uint8_t *bc_rest);

eval_fun evaluators[] =
  { eval_fst,
    eval_snd,
    eval_add,
    eval_rest,
    eval_push,
    eval_swap,
    eval_loadi,
    eval_loadb,
    eval_clear,
    eval_cons,
    eval_cur,
    eval_pack,
    eval_skip,
    eval_stop,
    eval_app,
    eval_return,
    eval_call,
    eval_goto,
    eval_gotofalse,
    eval_switch,
    eval_abs,
    eval_neg,
    eval_not,
    eval_dec,
    eval_addi,
    eval_muli,
    eval_mini,
    eval_addf,
    eval_mulf,
    eval_minf,
    eval_gt,
    eval_lt,
    eval_eq,
    eval_ge,
    eval_le };



int eval_fst(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_snd(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_add(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_rest(vmc_t *vmc, uint8_t *bc_rest)  {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_push(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_swap(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_loadi(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_loadb(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_clear(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_cons(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_cur(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_pack(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_skip(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_stop(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_app(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_return(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_call(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_goto(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_gotofalse(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_switch(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_abs(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_neg(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_not(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_dec(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_addi(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_muli(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_mini(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_addf(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_mulf(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_minf(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_gt(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_lt(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_eq(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_ge(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}

int eval_le(vmc_t *vmc, uint8_t *bc_rest) {
  (void)vmc;
  (void)bc_rest;
  return 1;
}
