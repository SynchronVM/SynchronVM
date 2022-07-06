#include<foreign.h>
#include<typedefs.h>
cam_value_t foreign_add(cam_value_t a, cam_value_t b){
  (void)a;
  (void)b;
  cam_value_t v;
  v.flags = 0;
  v.value = 0;
  return v;
}

cam_value_t foreign_sub(cam_value_t a, cam_value_t b){
  (void)a;
  (void)b;
  cam_value_t v;
  v.flags = 0;
  v.value = 0;
  return v;
}

