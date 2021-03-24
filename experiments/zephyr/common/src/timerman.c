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

<<<<<<< HEAD:experiments/zephyr/common/src/timerman.c
#include <timerman.h>
=======
#ifndef __FORMAL_ENV_H_
#define __FORMAL_ENV_H_

typedef enum {
  PAT_VAR,
  PAT_NIL,
  PAT_TUP,
  PAT_LAY
} pat_type;

struct pat_s;

typedef struct {
  struct pat_s *fst;
  struct pat_s *snd;
} pat_tup_t;

typedef struct {
  char *var;
  struct pat_s *as;
} pat_lay_t;

typedef struct pat_s {
  pat_type type;
  union { 
    char *var;
    pat_tup_t tup;
    pat_lay_t lay;
  } pat;
} pat_t; 

typedef struct formal_env_s {
  pat_t *head;
  struct formal_env_s *tail;
} formal_env_t;

extern pat_t *pat_var(char *var);
extern pat_t *pat_nil();
extern pat_t *pat_tup(pat_t *fst, pat_t *snd);
extern pat_t *pat_lay(char *var, pat_t *p);

#endif
>>>>>>> tiny_camiot:frontend/tiny_camiot/include/formal_env.h
