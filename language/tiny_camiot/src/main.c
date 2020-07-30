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


struct varlist {
  struct varlist *next;
  char   *var;
} varlist_t;

/* Compile time environment of variables. 
   used to compute the index of the variable to be able instantiate the
   correct "Acc" (access) instruction.  
*/
varlist_t *varlist_add(varlist_t *list, char *var) {

  varlist_t *new = malloc(sizeof(varlist));
  if (!new) return NULL;
  new->next = list;
  new->var  = var;
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
  
