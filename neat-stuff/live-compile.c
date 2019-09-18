#include <stdlib.h>
#include <stdio.h>
#include <sys/mman.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>

void * alignptr(void * ptr, uintptr_t alignment)
{
    return (void *)((uintptr_t)ptr & ~(alignment - 1));
}

unsigned char* read_file_into_heap(unsigned char* filename) {

    FILE* fp = fopen(filename,"rb");

    // seek end to get size of file
    fseek(fp, 0L, SEEK_END);
    int size = ftell(fp);
    rewind(fp);

    // make the place in memory for our code
    unsigned char* buffer = malloc(size * sizeof(unsigned char));
    
    // read binary file into memoery
    fread(buffer, sizeof *buffer, size, fp);

    // make this section executiable
    void *p = alignptr((void*)buffer, 4096);
    int result = mprotect(p, 4096, PROT_READ | PROT_WRITE | PROT_EXEC);
    if (result == -1) { printf("Error: %s\n", strerror(errno)); }

    return buffer;
}

void print_bytes(unsigned char* start, int n) {
    for (int i = 0; i < n; i++) {
        if (i % 16 == 0) {
            printf("%p  ", start + i);
        }

        printf("%02x ", start[i]);

        if (i % 16 == 15) {
            printf("\n");
        }
    }
    printf("\n");
}

typedef int (*operator_t)(int);
typedef int (*inter_t)();

int compile_no_arg_inter(char* code_string) {

    int status;
    FILE* fp = popen("gcc -g -c -xc - -o foo.o", "w");
    fputs(code_string, fp);
    pclose(fp);
    status = system("objcopy -O binary --only-section=.text foo.o foo.code");

    unsigned char* foo_code = read_file_into_heap("foo.code");
    //print_bytes(foo_code, 6);

    inter_t foo_fun = (inter_t)foo_code;
    return foo_fun();
}

int main() {

    int size = 100;
    char* code = malloc(size*sizeof(char));
    const char* temp = "int foo() {  return      ";
    strncpy(code, temp, size*sizeof(char));
    //printf("\n Pre code code: \n%s\n", code);
    int i = strlen(temp) - 1;
    char c = '\0';
    printf(">>> ");
    while (c != '\n'){
        c = getchar();

        //printf("Char: %d %c\n", i, c);
        code[i] = c;
        i++;
        if (i >= size) {
            //printf("reallocating to bigger\n");
            size = size * 2;
            code = realloc(code, size);
        }
    }
    code[i++] = ';';
    code[i++] = '}';
    code[i++] = '\n';

    //printf("\n Your code: \n%s\n", code);
    int result = compile_no_arg_inter(code);
    printf("%d\n", result);

    main();

    return 0;
}
