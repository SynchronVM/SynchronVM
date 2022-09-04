#include <foreign.h>
#include <stdio.h>
#include <typedefs.h>
#include "out.constr"


cam_value_t foreign_prnLst(cam_value_t x){

  int arr[2];
  int i = 0;
  cam_value_t temp = x;

  while(true){

    if(is_pointer(&temp)){
      cam_value_t f = cvt_get_fst(&temp);
      if(is_constructor(f.value,"Nil")){
        break;
      } else if(is_constructor(f.value, "Cons")){
        cam_value_t s = cvt_get_snd(&temp);
        arr[i] = cvt_get_fst(&s).value;
        i++;
        temp = cvt_get_snd(&s);
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

cam_value_t foreign_allocLst(cam_value_t x){
  cam_value_t ptr0 = alloc_cvt();
  cam_value_t ptr1 = alloc_cvt();
  cam_value_t ptr2 = alloc_cvt();
  cam_value_t ptr3 = alloc_cvt();
  cam_value_t ptr4 = alloc_cvt();
  cam_value_t cons = create_constructor("Cons");
  cam_value_t nil  = create_constructor("Nil");
  cam_value_t val0  = {.value = 5, .flags = 0};
  cam_value_t val1  = {.value = 3, .flags = 0};
  cvt_set(&ptr0, &cons, &ptr1);
  cvt_set(&ptr1, &val0, &ptr2);
  cvt_set(&ptr2, &cons, &ptr3);
  cvt_set(&ptr3, &val1, &ptr4);
  cvt_set_fst(&ptr4, &nil);
  //cvt_set_snd doesn't matter, it is a garbage value
	return ptr0;
}
