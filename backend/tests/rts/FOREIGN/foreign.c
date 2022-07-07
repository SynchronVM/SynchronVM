#include<foreign.h>
#include<typedefs.h>
cam_value_t foreign_add(cam_value_t *args){
  cam_value_t v;
  v.flags = 0;
  v.value = args[0].value + args[1].value;
  return v;
}


