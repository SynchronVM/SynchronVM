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

#include <stdlib.h>

#include <formal_env.h>

pat_t *pat_var(char *var) {

  pat_t *pat = malloc(sizeof(pat_t));
  if (!pat) return NULL;

  pat->type = PAT_VAR;
  pat->pat.var = var; /* maybe copy string? */

  return pat;
}

pat_t *pat_nil() {

  pat_t *pat = malloc(sizeof(pat_t));
  if (!pat) return NULL;

  pat->type = PAT_NIL;

  return pat;
}

pat_t *pat_tup(pat_t *fst, pat_t *snd) {

  pat_t *pat = malloc(sizeof(pat_t));
  if (!pat) return NULL;

  pat->type = PAT_TUP;
  pat->pat.tup.fst = fst;
  pat->pat.tup.snd = snd;

  return pat;
}

pat_t *pat_lay(char *var, pat_t *p) {

  pat_t *pat = malloc(sizeof(pat_t));
  if (!pat) return NULL;

  pat->type = PAT_LAY;
  pat->pat.lay.var = var;
  pat->pat.lay.as  = p;

  return pat;
}

