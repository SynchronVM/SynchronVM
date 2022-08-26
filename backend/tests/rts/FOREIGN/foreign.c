#include<foreign.h>
#include<stdio.h>
#include<typedefs.h>
#include "out.constr"
cam_value_t foreign_prnLst(cam_value_t x){

  int arr[2];
  int i = 0;
  cam_value_t temp = x;

  while(true){

    if(is_pointer(&temp)){
      cam_value_t f = cvt_fst(&temp);
      if(is_constructor(f.value,"Nil2")){
        break;
      } else if(is_constructor(f.value, "Cons1")){
        cam_value_t s = cvt_snd(&temp);
        arr[i] = cvt_fst(&s).value;
        i++;
        temp = cvt_snd(&s);
      }
    }
  }
  printf("Printing List \n");
  for (int j = 0; j<2; j++){
    printf("%d\n",arr[j]);
  }

  cam_value_t abc ={.value = 0, .flags = 0};
  return abc;
}


