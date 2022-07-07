#include<foreign.h>
#include<typedefs.h>
cam_value_t foreign_add(cam_value_t x, cam_value_t y){
  cam_value_t v;
  v.flags = 0;
  v.value = x.value + y.value;
  return v;
}


