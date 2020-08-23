

#include <stdio.h>
#include <stdlib.h>


#include <VMC.h>

int main(int argc, char **argv) {
  (void) argc;
  (void) argv;


  if (!vmc_init()) return 0;

  printf("%s\n", vm_containers[0].code_memory);


  return 1;
}
