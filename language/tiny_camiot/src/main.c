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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Parser.h"
#include "Printer.h"
#include "Absyn.h"


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

int main(int argc, char **argv) {


  FILE *fp;
  Exp ast;
  char *fn;

  if (argc != 2) {
    printf("Error: specify exactly one argument (file to compile)\n");
    return 1;
  }

  fn = argv[1];

  if (fn) {
    fp = fopen(fn, "r");
    if (!fp) {
      printf("Error: cannot open file %s\n", fn);
      return 1;
    }
    
    ast = pExp(fp);
    
    if (ast) {
      printf("%s\n", showExp(ast));
    } else {
      printf("Failure!\n");
      return 1;
    }
  } else {
    printf("Error: filename\n");
    return 1;
  }
    
  return 0;
}
  
