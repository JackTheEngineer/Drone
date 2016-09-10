/*
 * vector_tester.h
 *
 *  Created on: Jul 31, 2016
 *      Author: chocolate
 */

#ifndef TEST_SUPPORT_VECTOR_TESTER_H_
#define TEST_SUPPORT_VECTOR_TESTER_H_

#include "base.h"
#include "vector_operations.h"

void print_vector(Vector_t *vector, const char *name);
void Test_vectors_equal(Vector_t * vector_1, Vector_t * vector_2);

#endif /* TEST_SUPPORT_VECTOR_TESTER_H_ */
