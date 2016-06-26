/*
 * test_vector_operations.c
 *
 *  Created on: May 14, 2016
 *      Author: jakov
 */

#include "test_helper.h"
#include "vector_operations.h"


TEST_GROUP(vector_operations);
Vector_t vector_contianer;
Vector_t *vect = &vector_contianer;

Vector_t vect_1_container;
Vector_t *vect_1 = &vect_1_container;

Vector_t vect_2_container;
Vector_t *vect_2 = &vect_2_container;


_STATIC_ void Test_vect_values_equal_to(double value);
_STATIC_ void Test_vectors_equal(Vector_t * vector_1, Vector_t * vector_2);

TEST_SETUP(vector_operations){
    Vect_set_all_values_to(vect, 0.0);
    Vect_set_all_values_to(vect_1, 0.0);
    Vect_set_all_values_to(vect_2, 0.0);
}

TEST_TEAR_DOWN(vector_operations){
}

TEST(vector_operations, kron_delta_should_return_zero_when_indizes_are_not_equal){
	TEST_ASSERT_EQUAL(0, kron_delta(1,0));
}

TEST(vector_operations, kron_delta_should_return_one_when_indizes_are_equal){
	TEST_ASSERT_EQUAL(1, kron_delta(10,10));
}

TEST(vector_operations, vect_val_from_ind_returns_x_value_on_ind_1){
	vect->x = 55;
	TEST_ASSERT_EQUAL(55, Vect_read(vect, 1));
}

TEST(vector_operations, vect_val_from_ind_returns_y_value_on_ind_2){
	vect->y = 77;
	TEST_ASSERT_EQUAL(77, Vect_read(vect, 2));
}

TEST(vector_operations, vect_val_from_ind_returns_z_value_on_ind_3){
	vect->z = 99;
	TEST_ASSERT_EQUAL(99, Vect_read(vect, 3));
}

TEST(vector_operations, vect_val_from_ind_returns_zero_if_index_zero){
	TEST_ASSERT_EQUAL(0, Vect_read(vect, 0));
}

TEST(vector_operations, vect_val_from_ind_returns_zero_if_index_bigger3){
	TEST_ASSERT_EQUAL(0, Vect_read(vect, 4));
}

TEST(vector_operations, set_all_values_to_should_work){
    vect->x = 1;
    vect->y = 1; 
    vect->z = 1; 
    Vect_set_all_values_to(vect, 0.0);
    Test_vect_values_equal_to(0.0);
}

TEST(vector_operations, set_all_values_to_should_work2){
    vect->x = 2.0;
    vect->y = 2.0; 
    vect->z = 2.0; 
    Vect_set_all_values_to(vect, 1.0);
    Test_vect_values_equal_to(1.0);
}

TEST(vector_operations, Vect_write_at_x_index_should_work){
	vect->x = 0;
	Vect_write(vect, 1, 1.0);
	TEST_ASSERT_EQUAL_DOUBLE(1.0, Vect_read(vect, 1));
}

TEST(vector_operations, Vect_write_at_y_index_should_work){
	vect->y = 0;
	Vect_write(vect, 2, 2.0);
	TEST_ASSERT_EQUAL_DOUBLE(2.0, Vect_read(vect, 2));
}


TEST(vector_operations, Vect_write_at_y_index_should){
    	vect->z= 0;
	Vect_write(vect, 3, 2.0);
	TEST_ASSERT_EQUAL_DOUBLE(2.0, Vect_read(vect, 3));
}

TEST(vector_operations, Vect_write_should_change_only_value_index){
    vect->x = 4.0;
    vect->y = 4.0;
    vect->z = 4.0;
    
    Vect_write(vect, 1, 2.0);
   
    TEST_ASSERT_EQUAL_DOUBLE(2.0, vect->x);
    TEST_ASSERT_EQUAL_DOUBLE(4.0, vect->y);
    TEST_ASSERT_EQUAL_DOUBLE(4.0, vect->z);
}

TEST(vector_operations, Vect_add_should_add_first_component){
    uint8_t i;

    for(i = 0; i < 10; i++){  
        Vect_write(vect_1, 1, (double)(i));
        Vect_write(vect_2, 1, (double)(i));
        Vect_add(vect_1, vect_2, vect);
        TEST_ASSERT_EQUAL_DOUBLE((double)2*i, Vect_read(vect,1));
    }
}

TEST(vector_operations, Vect_add_should_add_second_component){
    uint8_t i;

    for(i = 0; i < 10; i++){  
        Vect_write(vect_1, 2, (double)(i));
        Vect_write(vect_2, 2, (double)(i));
        Vect_add(vect_1, vect_2, vect);
        TEST_ASSERT_EQUAL_DOUBLE((double)2*i, Vect_read(vect,2));
    }
}

TEST(vector_operations, Vect_add_should_add_third_component){
    uint8_t i;

    for(i = 0; i < 10; i++){  
        Vect_write(vect_1, 3, (double)(i));
        Vect_write(vect_2, 3, (double)(i));
        Vect_add(vect_1, vect_2, vect);
        TEST_ASSERT_EQUAL_DOUBLE((double)2*i, Vect_read(vect,3));
    }
}

TEST(vector_operations, Vect_uniform_should_return_a_vector_with_length_1){
    Vect_set_all_values_to(vect_1, 5.0);
    Vect_uniform(vect_1, vect);
    TEST_ASSERT_EQUAL_DOUBLE(1.0, Vect_length(vect));
}

TEST(vector_operations, Vect_uniform_should_return_the_same_vector_with_a_uniform_vector){
    uint8_t i;
    for(i=1; i<=3; i++){
        Vect_set_all_values_to(vect_1, 0);
        Vect_write(vect_1, i, 1.0);
        Vect_uniform(vect_1, vect);
        Test_vectors_equal(vect, vect_1);
    }
}

TEST(vector_operations, Vect_length_should_return_one_with_vector_magnitude_1){
    uint8_t i;
    for(i=1; i<=3; i++){
        Vect_set_all_values_to(vect, 0.0);
        Vect_write(vect, i, 1.0);
        printf("vect: %f \n", Vect_length(vect));
        TEST_ASSERT_EQUAL_DOUBLE(1.0, Vect_length(vect));
    }
}

TEST(vector_operations, Vect_length_should_return_5_with_components_3_and_4){
    Vect_write(vect, 1, 3.0);
    Vect_write(vect, 2, 4.0);
    TEST_ASSERT_EQUAL_DOUBLE(5.00, Vect_length(vect));
}

TEST(vector_operations, Vect_length_should_return_1_with_each_component_sqrt_3){
    double sqrt_val = 1/sqrt(3);
    Vect_set_all_values_to(vect, sqrt_val);
    TEST_ASSERT_EQUAL_DOUBLE(1.0, Vect_length(vect));
}

TEST(vector_operations, Vect_write_three_values_should_work){
    uint8_t i;
    uint8_t j;
    uint8_t k;
    for(i = 0; i <= 5; i++){
        for(j = 0; j <= 5; j++){
            for(k = 0; k <= 5; k++){
                Vect_write_three_values(vect, (double)i,(double)j,(double)k);
                TEST_ASSERT_EQUAL_DOUBLE((double)i, Vect_read(vect, 1));
                TEST_ASSERT_EQUAL_DOUBLE((double)j, Vect_read(vect, 2));
                TEST_ASSERT_EQUAL_DOUBLE((double)k, Vect_read(vect, 3));
            }
        }
    }
}

TEST(vector_operations, Vect_multiply_with_const_should_work){
    uint8_t i;
    
    for(i=1; i < 10; i++){
        Vect_set_all_values_to(vect, 0);
        Vect_write_three_values(vect, 1.0, 2.0, 3.0);
        Vect_multiply(vect, (double)i);
        TEST_ASSERT_EQUAL_DOUBLE(1.0 * (double)i, Vect_read(vect, 1));
        TEST_ASSERT_EQUAL_DOUBLE(2.0 * (double)i, Vect_read(vect, 2));
        TEST_ASSERT_EQUAL_DOUBLE(3.0 * (double)i, Vect_read(vect, 3));
    }
}

TEST(vector_operations, Vect_copy_should_copy_vectors){
    Vect_write_three_values(vect, 1.0, 2.0, 4.5);
    Vect_copy_from_to(vect, vect_1);
    Test_vectors_equal(vect, vect_1);
}

_STATIC_ void Test_vect_values_equal_to(double value){
    TEST_ASSERT_EQUAL_DOUBLE(value , vect->x);
    TEST_ASSERT_EQUAL_DOUBLE(value , vect->y);
    TEST_ASSERT_EQUAL_DOUBLE(value , vect->z);
}

_STATIC_ void Test_vectors_equal(Vector_t * vector_1, Vector_t * vector_2){
    TEST_ASSERT_EQUAL_DOUBLE(Vect_read(vector_1, 1), Vect_read(vector_2, 1));
    TEST_ASSERT_EQUAL_DOUBLE(Vect_read(vector_1, 2), Vect_read(vector_2, 2));
    TEST_ASSERT_EQUAL_DOUBLE(Vect_read(vector_1, 3), Vect_read(vector_2, 3));
}

