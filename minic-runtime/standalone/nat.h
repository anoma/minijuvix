#ifndef NAT_H_
#define NAT_H_

#include <stdbool.h>
#define prim_zero 0

int prim_suc(int n) {
    return n + 1;
}

bool is_prim_zero(int n) {
    return n == 0;
}

bool is_prim_suc(int n) {
    return n != 0;
}

int proj_ca0_prim_suc(int n) {
    return n - 1;
}

int prim_natplus(int a, int b) {
    return a + b;
}


#endif // NAT_H_
