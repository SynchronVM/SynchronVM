#include<string.h>
bool is_constructor(uint16_t tagidx, char *constr) {
    switch(tagidx) {
        case 1: {
            return strncmp(constr, "Cons1", strlen(constr)) == 0;
            break;
        }

        case 0: {
            return strncmp(constr, "Nil2", strlen(constr)) == 0;
            break;
        }

        default: return false;
    }
    return false;
}
