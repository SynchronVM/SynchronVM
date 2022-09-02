#include <foreign.h>
#include <stdio.h>
#include <typedefs.h>
#include "out.constr"
cam_value_t foreign_prnLst2(cam_value_t x)
{

  int arr[2];
  int i = 0;
  cam_value_t temp = x;

  while (true)
  {

    if (is_pointer(&temp))
    {
      cam_value_t f = cvt_get_fst(&temp);
      if (is_constructor(f.value, "Nil"))
      {
        printf("nil\n");
        break;
      }
      else if (is_constructor(f.value, "Cons"))
      {
        printf("cons\n");
        cam_value_t s = cvt_get_snd(&temp);
        arr[i] = cvt_get_fst(&s).value;
        i++;
        temp = cvt_get_snd(&s);
      }
    }
    printf("Printing List \n");
    for (int j = 0; j < 2; j++)
    {
      printf("%d\n", arr[j]);
    }

    cam_value_t abc = {.value = 0, .flags = 0};
    return abc;
  }
}

// assume input is a list of int between values 0 and 20
cam_value_t foreign_prnLst(cam_value_t x)
{
  int histogram[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  cam_value_t temp = x;
  while (true)
  {
    if (is_pointer(&temp))
    {
      cam_value_t fst = cvt_get_fst(&temp);
      cam_value_t snd = cvt_get_snd(&temp);
      if (is_constructor(fst.value, "Nil"))
      {
        break;
      }
      else if (is_constructor(fst.value, "Cons"))
      {
        histogram[cvt_get_fst(&snd).value] = histogram[cvt_get_fst(&snd).value] + 1;
        temp = cvt_get_snd(&snd);
      }
    }
  }

  for (int i = 0; i < 20; i++)
  {
    printf("number of %ds: %d\n", i, histogram[i]);
  }

  int numres = 0;
  for (int i = 0; i < 20; i++)
  {
    if (histogram[i])
      numres++;
  }

  printf("%d values were non-zero\n", numres);

  cam_value_t res_ptr = alloc_cvt();
  cam_value_t nil = create_constructor("Nil");
  cvt_set_fst(&res_ptr, &nil);

  for (int i = 19; i >= 0; i--)
  {
    if (histogram[i] != 0)
    {
      printf("value %d occurred %d times\n", i, histogram[i]);
      cam_value_t val_ptr = alloc_cvt();
      cam_value_t val = {.value = histogram[i], .flags = 0};
      cvt_set(&val_ptr, &val, &res_ptr);

      cam_value_t cons_ptr = alloc_cvt();
      cam_value_t cons = create_constructor("Cons");
      cvt_set(&cons_ptr, &cons, &val_ptr);

      res_ptr = cons_ptr;
    }
  }
  printf("is_ptr(res_ptr): %d\n", is_pointer(&res_ptr));
  foreign_prnLst2(res_ptr);

  return res_ptr;
  // create histogram to return to vm
}
