#ifndef NAT_H_
#define NAT_H_

#include <stdbool.h>
#define zero 0

int suc(int n) {
    return n + 1;
}

bool is_zero(int n) {
    return n == 0;
}

bool is_suc(int n) {
    return n != 0;
}

int proj_ca0_suc(int n) {
    return n - 1;
}

int natplus(int a, int b) {
    return a + b;
}


#endif // NAT_H_
