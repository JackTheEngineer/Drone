/*
 * test_vector_operations.c
 *
 *  Created on: May 14, 2016
 *      Author: jakov
 */

#include "test_helper.h"
#include "vector_operations.h"
#include "vector_tester.h"

TEST_GROUP(vector_operations);

POINTER_TO_CONTAINER(Vector_t, vect);
POINTER_TO_CONTAINER(Vector_t, vect_1);
POINTER_TO_CONTAINER(Vector_t, vect_2);

_STATIC_ void Test_vect_values_equal_to(_FLOAT_ value);
_STATIC_ void Set_Test_vectors_zero(void);
_STATIC_ void _write_5_6_7_into_each_vector(Vector_t *vectorlist, uint8_t length);

TEST_SETUP(vector_operations){
    Set_Test_vectors_zero();
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
	vect->v[0] = 55;
	TEST_ASSERT_EQUAL(55, Vect_read(vect, 1));
}

TEST(vector_operations, vect_val_from_ind_returns_y_value_on_ind_2){
	vect->v[1] = 77;
	TEST_ASSERT_EQUAL(77, Vect_read(vect, 2));
}

TEST(vector_operations, vect_val_from_ind_returns_z_value_on_ind_3){
	vect->v[2] = 99;
	TEST_ASSERT_EQUAL(99, Vect_read(vect, 3));
}

TEST(vector_operations, set_all_values_to_should_work){
    vect->v[0] = 1;
    vect->v[1] = 1; 
    vect->v[2] = 1; 
    Vect_set_all_values_to(vect, 0.0);
    Test_vect_values_equal_to(0.0);
}

TEST(vector_operations, set_all_values_to_should_work2){
    vect->v[0] = 2.0;
    vect->v[1] = 2.0; 
    vect->v[2] = 2.0; 
    Vect_set_all_values_to(vect, 1.0);
    Test_vect_values_equal_to(1.0);
}

TEST(vector_operations, Vect_write_at_x_index_should_work){
	vect->v[0] = 0;
	Vect_write(vect, 1, 1.0);
	TEST_ASSERT_EQUAL_DOUBLE(1.0, Vect_read(vect, 1));
}

TEST(vector_operations, Vect_write_at_y_index_should_work){
	vect->v[1] = 0;
	Vect_write(vect, 2, 2.0);
	TEST_ASSERT_EQUAL_DOUBLE(2.0, Vect_read(vect, 2));
}


TEST(vector_operations, Vect_write_at_y_index_should){
    	vect->v[2]= 0;
	Vect_write(vect, 3, 2.0);
	TEST_ASSERT_EQUAL_DOUBLE(2.0, Vect_read(vect, 3));
}

TEST(vector_operations, Vect_write_should_change_only_value_index){
    vect->v[0] = 4.0;
    vect->v[1] = 4.0; 
    vect->v[2] = 4.0; 
    Vect_write(vect, 1, 2.0);
   
    TEST_ASSERT_EQUAL_DOUBLE(2.0, vect->v[0]);
    TEST_ASSERT_EQUAL_DOUBLE(4.0, vect->v[1]);
    TEST_ASSERT_EQUAL_DOUBLE(4.0, vect->v[2]);
}

TEST(vector_operations, Vect_add_should_add_first_component){
    uint8_t i;

    for(i = 0; i < 10; i++){  
        Vect_write(vect_1, 1, (_FLOAT_)(i));
        Vect_write(vect_2, 1, (_FLOAT_)(i));
        Vect_add(vect_1, vect_2, vect);
        TEST_ASSERT_EQUAL_DOUBLE((_FLOAT_)2*i, Vect_read(vect,1));
    }
}

TEST(vector_operations, Vect_add_should_add_second_component){
    uint8_t i;

    for(i = 0; i < 10; i++){  
        Vect_write(vect_1, 2, (_FLOAT_)(i));
        Vect_write(vect_2, 2, (_FLOAT_)(i));
        Vect_add(vect_1, vect_2, vect);
        TEST_ASSERT_EQUAL_DOUBLE((_FLOAT_)2*i, Vect_read(vect,2));
    }
}

TEST(vector_operations, Vect_add_should_add_third_component){
    uint8_t i;

    for(i = 0; i < 10; i++){  
        Vect_write(vect_1, 3, (_FLOAT_)(i));
        Vect_write(vect_2, 3, (_FLOAT_)(i));
        Vect_add(vect_1, vect_2, vect);
        TEST_ASSERT_EQUAL_DOUBLE((_FLOAT_)2*i, Vect_read(vect,3));
    }
}

TEST(vector_operations, Vect_uniform_should_return_a_vector_with_length_1){
    Vect_set_all_values_to(vect_1, 5.0);
    Vect_uniform(vect_1, vect);
    TEST_ASSERT_DOUBLE_WITHIN(0.000001, 1.0, Vect_length(vect));
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
    _FLOAT_ sqrt_val = 1/sqrt(3);
    Vect_set_all_values_to(vect, sqrt_val);
    TEST_ASSERT_DOUBLE_WITHIN(0.000001, 1.0, Vect_length(vect));
}

TEST(vector_operations, Vect_write_three_values_should_work){
    uint8_t i;
    uint8_t j;
    uint8_t k;
    for(i = 0; i <= 5; i++){
        for(j = 0; j <= 5; j++){
            for(k = 0; k <= 5; k++){
                Vect_write_three_values(vect, (_FLOAT_)i,(_FLOAT_)j,(_FLOAT_)k);
                TEST_ASSERT_EQUAL_DOUBLE((_FLOAT_)i, Vect_read(vect, 1));
                TEST_ASSERT_EQUAL_DOUBLE((_FLOAT_)j, Vect_read(vect, 2));
                TEST_ASSERT_EQUAL_DOUBLE((_FLOAT_)k, Vect_read(vect, 3));
            }
        }
    }
}

TEST(vector_operations, Vect_multiply_with_const_should_work){
    uint8_t i;
    
    for(i=1; i < 10; i++){
        Vect_set_all_values_to(vect, 0);
        Vect_set_all_values_to(vect_1, 0);
        Vect_write_three_values(vect, 1.0, 2.0, 3.0);
        Vect_times_const(vect, (_FLOAT_)i, vect_1);
        TEST_ASSERT_EQUAL_DOUBLE(1.0 * (_FLOAT_)i, Vect_read(vect_1, 1));
        TEST_ASSERT_EQUAL_DOUBLE(2.0 * (_FLOAT_)i, Vect_read(vect_1, 2));
        TEST_ASSERT_EQUAL_DOUBLE(3.0 * (_FLOAT_)i, Vect_read(vect_1, 3));
    }
}

TEST(vector_operations, Vect_copy_should_copy_vectors){
    Vect_write_three_values(vect, 1.0, 2.0, 4.5);
    Vect_copy_from_to(vect, vect_1);
    Test_vectors_equal(vect, vect_1);
}

TEST(vector_operations, Vect_cross_multiply_index_1_should_follow_rule){
    _FLOAT_ i;
    _FLOAT_ j;
    _FLOAT_ k;
    _FLOAT_ l;

    for(i = 0.0; i <= 6.0; i = i+1.0){
        for(j = 0.0; j <= 6.0; j = j+1.0){
            k = i+1;
            l = j+1;

            Vect_set_all_values_to(vect, 0);            
            Vect_write_three_values(vect_1, 0, i, j);
            Vect_write_three_values(vect_2, 0, k, l);
            
            Vect_cross_multiply(vect_1, vect_2, vect);

            TEST_ASSERT_EQUAL_DOUBLE(Vect_read(vect, 1), i*l - k*j);
        }
    }
}

TEST(vector_operations, Vect_cross_multiply_index_2_should_follow_rule){
    _FLOAT_ i;
    _FLOAT_ j;
    _FLOAT_ k;
    _FLOAT_ l;

    for(i = 0.0; i <= 6.0; i = i+1.0){
        for(j = 0.0; j <= 6.0; j = j+1.0){
            k = i+1;
            l = j+1;

            Vect_set_all_values_to(vect, 0);            
            Vect_write_three_values(vect_1, j, 0, i);
            Vect_write_three_values(vect_2, l, 0, k);
            
            Vect_cross_multiply(vect_1, vect_2, vect);

            TEST_ASSERT_EQUAL_DOUBLE(Vect_read(vect, 2), i*l - k*j);
        }
    }
}

TEST(vector_operations, Vect_cross_multiply_index_3_should_follow_rule){
    _FLOAT_ i;
    _FLOAT_ j;
    _FLOAT_ k;
    _FLOAT_ l;

    for(i = 0.0; i <= 6.0; i = i+1.0){
        for(j = 0.0; j <= 6.0; j = j+1.0){
            k = i+1;
            l = j+1;

            Vect_set_all_values_to(vect, 0);            
            Vect_write_three_values(vect_1, i, j, 0);
            Vect_write_three_values(vect_2, k, l, 0);
            
            Vect_cross_multiply(vect_1, vect_2, vect);

            TEST_ASSERT_EQUAL_DOUBLE(Vect_read(vect, 3), i*l - k*j);
        }
    }
}

TEST(vector_operations, sum_up_list_of_Vectors_should_work){
	Vector_t vectorlist[5];
	POINTER_TO_CONTAINER(Vector_t, sum_vector);
	POINTER_TO_CONTAINER(Vector_t, compare_vector);

	_write_5_6_7_into_each_vector(vectorlist, 5);
	Vect_write_three_values(compare_vector, 25.0, 30.0, 35.0);
	
	Vect_sum_up_list_of_vectors(vectorlist, sum_vector, 5);
	
	Test_vectors_equal(compare_vector, sum_vector);
}

TEST(vector_operations, get_pointer_of_indexvalue_1){
	_FLOAT_ *ptr;
	_FLOAT_ *compare_ptr;

	compare_ptr = &(vect->v[0]);
	ptr = Vect_pointer_to_index(vect, 1);

	TEST_ASSERT_POINTERS_EQUAL(compare_ptr, ptr);
}

TEST(vector_operations, get_pointer_of_indexvalue_2){
	_FLOAT_ *ptr;
	_FLOAT_ *compare_ptr;

	compare_ptr = &(vect->v[1]);
	ptr = Vect_pointer_to_index(vect, 2);

	TEST_ASSERT_POINTERS_EQUAL(compare_ptr, ptr);
}

TEST(vector_operations, get_pointer_of_indexvalue_3){
	_FLOAT_ *ptr;
	_FLOAT_ *compare_ptr;

	compare_ptr = &(vect->v[2]);
	ptr = Vect_pointer_to_index(vect, 3);

	TEST_ASSERT_POINTERS_EQUAL(compare_ptr, ptr);
}


_STATIC_ void Test_vect_values_equal_to(_FLOAT_ value){
    TEST_ASSERT_EQUAL_DOUBLE(value , vect->v[0]);
    TEST_ASSERT_EQUAL_DOUBLE(value , vect->v[1]);
    TEST_ASSERT_EQUAL_DOUBLE(value , vect->v[2]);
}

_STATIC_ void Set_Test_vectors_zero(void){
    Vect_set_all_values_to(vect_1, 0.0);
    Vect_set_all_values_to(vect_2, 0.0);
    Vect_set_all_values_to(vect, 0.0);
}

_STATIC_ void _write_5_6_7_into_each_vector(Vector_t *vectorlist, uint8_t length){
	uint8_t i;

	for(i=0; i<length; i++){
		Vect_write_three_values(&(vectorlist[i]), 5, 6, 7);
	}
}
