#include<foreign.h>
#include<typedefs.h>
cam_value_t foreign_add(cam_value_t a, cam_value_t b){
  cam_value_t v;
  v.flags = 0;
  v.value = a.value + b.value;
  return v;
}


