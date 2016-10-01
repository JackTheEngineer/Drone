/*
 * vector_tester.c
 *
 *  Created on: Jul 31, 2016
 *      Author: chocolate
 */

#include "test_helper.h"
#include "vector_tester.h"

void Test_vectors_equal(Vector_t * vector_1, Vector_t * vector_2){
	TEST_ASSERT_DOUBLE_WITHIN(0.000001, Vect_read(vector_1, 1), Vect_read(vector_2, 1));
	TEST_ASSERT_DOUBLE_WITHIN(0.000001, Vect_read(vector_1, 2), Vect_read(vector_2, 2));
	TEST_ASSERT_DOUBLE_WITHIN(0.000001, Vect_read(vector_1, 3), Vect_read(vector_2, 3));
}

void print_vector(Vector_t *vector, const char *name){
  uint8_t i;
  printf("%s\n", name);
  for(i=1; i<=3; i++){
    printf("%.3f\n", Vect_read(vector,i));
  }
}
