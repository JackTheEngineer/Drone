// generated test runner
#include "unity.h"
#include "unity_fixture.h"
#include <time.h>
#include <stdio.h>

static void run_all_tests(void);

//CONTENT

int main(int argc, const char * argv[]){
    int retval;
    clock_t begin, end;
    double time_spent;

    begin = clock();
    retval = UnityMain(argc, argv, run_all_tests);
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("Execution time %f\n",time_spent);
    return retval;
}
