/*
 * vector_tester.c
 *
 *  Created on: Jul 31, 2016
 *      Author: chocolate
 */

#include "test_helper.h"
#include "vector_tester.h"

void Test_vectors_equal(Vector_t * vector_1, Vector_t * vector_2){
    TEST_ASSERT_EQUAL_DOUBLE(Vect_read(vector_1, 1), Vect_read(vector_2, 1));
    TEST_ASSERT_EQUAL_DOUBLE(Vect_read(vector_1, 2), Vect_read(vector_2, 2));
    TEST_ASSERT_EQUAL_DOUBLE(Vect_read(vector_1, 3), Vect_read(vector_2, 3));
}

