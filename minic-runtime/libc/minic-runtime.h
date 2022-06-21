#ifndef MINIC_RUNTIME_H_
#define MINIC_RUNTIME_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef __UINTPTR_TYPE__ uintptr_t;

typedef struct minijuvix_function {
    uintptr_t fun;
} minijuvix_function_t;

char* intToStr(int i) {
    int length = snprintf(NULL, 0, "%d", i);
    char* str = (char*)malloc(length + 1);
    snprintf(str, length + 1, "%d", i);
    return str;
}

int putStr(const char* str) {
    fputs(str, stdout);
    return fflush(stdout);
}

int putStrLn(const char* str) {
    puts(str);
    return fflush(stdout);
}

#endif // MINIC_RUNTIME_H_
