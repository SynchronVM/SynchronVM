#include <foreign.h>
#include <stdio.h>
#include <typedefs.h>
#include "out.constr"


cam_value_t foreign_prnLst(cam_value_t x){

  cam_value_t temp = x;

  while(true){

    if(is_pointer(&temp)){
      cam_value_t f = cvt_get_fst(&temp);
      if(is_constructor(f.value,"Nil")){
        break;
      } else if(is_constructor(f.value, "Cons")){
        cam_value_t s = cvt_get_snd(&temp);
        printf("%d\n", cvt_get_fst(&s).value);
        temp = cvt_get_snd(&s);
      }
    }
  }

  cam_value_t abc ={.value = 0, .flags = 0};
  return abc;
}

/* cam_value_t foreign_allocLst(cam_value_t x){ */
/*   cam_value_t ptr0 = alloc_cvt(0); */
/*   cam_value_t ptr1 = alloc_cvt(0); */
/*   cam_value_t ptr2 = alloc_cvt(0); */
/*   cam_value_t ptr3 = alloc_cvt(0); */
/*   cam_value_t ptr4 = alloc_cvt(0); */
/*   cam_value_t cons = create_constructor("Cons"); */
/*   cam_value_t nil  = create_constructor("Nil"); */
/*   cam_value_t val0  = {.value = 5, .flags = 0}; */
/*   cam_value_t val1  = {.value = 3, .flags = 0}; */
/*   cvt_set(&ptr0, &cons, &ptr1); */
/*   cvt_set(&ptr1, &val0, &ptr2); */
/*   cvt_set(&ptr2, &cons, &ptr3); */
/*   cvt_set(&ptr3, &val1, &ptr4); */
/*   cvt_set_fst(&ptr4, &nil); */
/*   //cvt_set_snd doesn't matter, it is a garbage value */
/* 	return ptr0; */
/* } */

cam_value_t foreign_allocLst(cam_value_t x){
  cam_value_t cons = create_constructor("Cons");
  cam_value_t nil  = create_constructor("Nil");
  cam_value_t val0  = {.value = 5, .flags = 0};
  cam_value_t val1  = {.value = 3, .flags = 0};
  cam_value_t ptr0 = alloc_cvt(4, cons, nil, val0, val1);
  cam_value_t ptr1 = alloc_cvt(4, cons, nil, val0, val1);
  cam_value_t ptr2 = alloc_cvt(4, cons, nil, val0, val1);
  cam_value_t ptr3 = alloc_cvt(4, cons, nil, val0, val1);
  cam_value_t ptr4 = alloc_cvt(4, cons, nil, val0, val1);
  cvt_set(&ptr0, &cons, &ptr1);
  cvt_set(&ptr1, &val0, &ptr2);
  cvt_set(&ptr2, &cons, &ptr3);
  cvt_set(&ptr3, &val1, &ptr4);
  cvt_set_fst(&ptr4, &nil);
  //cvt_set_snd doesn't matter, it is a garbage value
	return ptr0;
}

cam_value_t foreign_create_histogram(cam_value_t x) {
  int histogram[256] = {0};

  cam_value_t tmp = x;
  while(true) {
    if(is_pointer(&tmp)) {
      cam_value_t constructor = cvt_get_fst(&tmp);
      if(is_constructor(constructor.value, "Nil")) {
        break;
      } else if(is_constructor(constructor.value, "Cons")) {
        cam_value_t cont = cvt_get_snd(&tmp);
        histogram[cvt_get_fst(&cont).value] = histogram[cvt_get_fst(&cont).value] + 1;
        tmp = cvt_get_snd(&cont);
      }
    }
  }

  cam_value_t cons = create_constructor("Cons");
  cam_value_t nil = create_constructor("Nil");
  cam_value_t root = alloc_cvt(0);
  cam_value_t last_ptr = root;

  for(int i = 0; i < 256; i++) {
    if(histogram[i] != 0) {
      cam_value_t val_ptr = alloc_cvt(1, root);
      cam_value_t next_ptr = alloc_cvt(2, root, val_ptr);
      cam_value_t val = {.value = histogram[i], .flags = 0};
      cvt_set(&val_ptr, &val, &next_ptr);
      cvt_set(&last_ptr, &cons, &val_ptr);
      last_ptr = next_ptr;
    }
  }
  cvt_set_fst(&last_ptr, &nil);
  return root;
}
