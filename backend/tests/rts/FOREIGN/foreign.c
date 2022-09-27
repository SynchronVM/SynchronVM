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
  cam_value_t val2  = {.value = 6, .flags = 0};
  cam_value_t val3  = {.value = 8, .flags = 0};
  cam_value_t val4  = {.value = 9, .flags = 0};
  cam_value_t val5  = {.value = 1, .flags = 0};
  cam_value_t val6  = {.value = 2, .flags = 0};
  cam_value_t val7  = {.value = 4, .flags = 0};
  cam_value_t ptr0 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7);
  cam_value_t ptr1 = alloc_cvt(11, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0);
  cam_value_t ptr2 = alloc_cvt(12, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1);
  cam_value_t ptr3 = alloc_cvt(13, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2);
  cam_value_t ptr4 = alloc_cvt(14, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3);
  cam_value_t ptr5 = alloc_cvt(15, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4);
  cam_value_t ptr6 = alloc_cvt(16, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5);
  cam_value_t ptr7 = alloc_cvt(17, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6);
  cam_value_t ptr8 = alloc_cvt(18, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7);
  cam_value_t ptr9 = alloc_cvt(19, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7, ptr8);
  cam_value_t ptr10 = alloc_cvt(20, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7, ptr8, ptr9);
  cam_value_t ptr11 = alloc_cvt(21, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7, ptr8, ptr9, ptr10);
  cam_value_t ptr12 = alloc_cvt(22, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7, ptr8, ptr9, ptr10, ptr11);
  cam_value_t ptr13 = alloc_cvt(23, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7, ptr8, ptr9, ptr10, ptr11, ptr12);
  cam_value_t ptr14 = alloc_cvt(24, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7, ptr8, ptr9, ptr10, ptr11, ptr12, ptr13);
  cam_value_t ptr15 = alloc_cvt(25, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7, ptr8, ptr9, ptr10, ptr11, ptr12, ptr13, ptr14);
  cam_value_t ptr16 = alloc_cvt(26, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7, ptr8, ptr9, ptr10, ptr11, ptr12, ptr13, ptr14, ptr15);




  /* when the below is uncommented and good33.cam is ran *WITH 208 bytes heap* the list is not
     printed; this is because the pointers get garbage collected and the whole structure of
     the list is lost. The above allocation, however, works perfectly fine with 208 bytes heap.
     the number 208 bytes was experimentally found
   */
  /* cam_value_t ptr0 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr1 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr2 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr3 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr4 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr5 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr6 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr7 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr8 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr9 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr10 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr11 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr12 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr13 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr14 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr15 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */
  /* cam_value_t ptr16 = alloc_cvt(10, cons, nil, val0, val1, val2, val3, val4, val5, val6, val7); */

  cvt_set(&ptr0, &cons, &ptr1);
  cvt_set(&ptr1, &val0, &ptr2);
  cvt_set(&ptr2, &cons, &ptr3);
  cvt_set(&ptr3, &val1, &ptr4);
  cvt_set(&ptr4, &cons, &ptr5);
  cvt_set(&ptr5, &val2, &ptr6);
  cvt_set(&ptr6, &cons, &ptr7);
  cvt_set(&ptr7, &val3, &ptr8);
  cvt_set(&ptr8, &cons, &ptr9);
  cvt_set(&ptr9, &val4, &ptr10);
  cvt_set(&ptr10, &cons, &ptr11);
  cvt_set(&ptr11, &val5, &ptr12);
  cvt_set(&ptr12, &cons, &ptr13);
  cvt_set(&ptr13, &val6, &ptr14);
  cvt_set(&ptr14, &cons, &ptr15);
  cvt_set(&ptr15, &val7, &ptr16);
  cvt_set_fst(&ptr16, &nil);
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

      cam_value_t tuple_ptr = alloc_cvt(3, val_ptr, next_ptr, root);
      cam_value_t val = {.value = histogram[i], .flags = 0};
      cam_value_t key = {.value = i, .flags = 0};
      cvt_set(&tuple_ptr, &key, &val);

      cvt_set(&val_ptr, &tuple_ptr, &next_ptr);
      cvt_set(&last_ptr, &cons, &val_ptr);
      last_ptr = next_ptr;
    }
  }
  cvt_set_fst(&last_ptr, &nil);
  return root;
}

cam_value_t foreign_print_tupleIntInt(cam_value_t x) {
  if(is_pointer(&x)) {
    cam_value_t a = cvt_get_fst(&x);
    cam_value_t b = cvt_get_snd(&x);
    printf("(%d,%d)\n", a.value, b.value);
  } else {
    // error
    printf("tried to print (Int, Int), but argument type is not a pointer\n");
  }
  cam_value_t retVal = {.value = 0, .flags = 0};
  return retVal;
}

cam_value_t foreign_print_tupleIntIntList(cam_value_t x){

  cam_value_t temp = x;

  while(true){
    if(is_pointer(&temp)){
      cam_value_t f = cvt_get_fst(&temp);
      if(is_constructor(f.value,"Nil")){
        break;
      } else if(is_constructor(f.value, "Cons")){
        cam_value_t s = cvt_get_snd(&temp);
        foreign_print_tupleIntInt(cvt_get_fst(&s));
        temp = cvt_get_snd(&s);
      }
    }
  }

  cam_value_t abc ={.value = 0, .flags = 0};
  return abc;
}
