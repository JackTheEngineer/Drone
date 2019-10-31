/*
 * timetasks.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */
#include "timetasks.h"

#include "pin_pulse.h"
#include "RFM75_driver.h"
#include "delay.h"
#include "led.h"
#include "joystick.h"

#define NUM_OF_MEASUREMENTS_TAKEN 10


typedef uint16_t* (*Retrieve_function_f)(void *target);

void calculate_average(uint16_t v[NUM_OF_MEASUREMENTS_TAKEN][4], uint16_t r[4]);
void mergeSort(uint16_t arr[], uint16_t l, uint16_t r);
void median_filter(void *target_val, void *buf_val, Retrieve_function_f retrieve);
uint16_t *retrieve_horizontal(void *target);
uint16_t *retrieve_vertical(void *target);

uint16_t adc_values[NUM_OF_MEASUREMENTS_TAKEN][4];
uint8_t count = 0;

void TimeTasks_run(uint32_t ticks, OS_t *os){
	uint8_t sendbytes[32] = {0}; // it seems like it's at least necessary to send 16 bytes for a stable transmission :( bah.
	uint32_t length = 32;

	if((ticks % 1) == 0){
		Joysticks_get_newest_values(adc_values[count]);
		count++;
		if(count >= NUM_OF_MEASUREMENTS_TAKEN){
			count = 0;
		}
	}
	if((ticks % 20) == 0){
		uint16_t averaged[4];
		calculate_average(adc_values, averaged);
		Joystick_serialize_data(averaged, sendbytes);

		turnOn();
		RC_Iface_CE_high();
		RFM75_Transmit_bytes(sendbytes,
							  	  &length,
								  50,
								  true);
		RC_Iface_CE_low();
		turnOff();
		LED_toggle();
	}
}

void calculate_average(uint16_t v[NUM_OF_MEASUREMENTS_TAKEN][4], uint16_t r[4]){
	for(uint8_t i=0; i<NUM_OF_MEASUREMENTS_TAKEN; i++){
		for(uint8_t j=0; j<4; j++){
			r[j] += v[i][j];
		}
	}
	for(uint8_t j=0; j<4; j++){
		r[j] /= NUM_OF_MEASUREMENTS_TAKEN;
	}
}

void median_filter(void *target_val, void *buf_val, Retrieve_function_f retrieve){
	uint16_t valuearray[NUM_OF_MEASUREMENTS_TAKEN];
	for(uint8_t i=0; i < NUM_OF_MEASUREMENTS_TAKEN; i++){
//		valuearray[i] = *((uint16_t *)retrieve(buf_val[i]));
	}

	mergeSort(valuearray, 1, NUM_OF_MEASUREMENTS_TAKEN);

	*(retrieve(target_val)) = valuearray[NUM_OF_MEASUREMENTS_TAKEN/2];
}

// Merges two subarrays of arr[].
// First subarray is arr[l..m]
// Second subarray is arr[m+1..r]
void merge(uint16_t arr[], uint16_t l, uint16_t m, uint16_t r)
{
    uint16_t i, j, k;
    uint16_t n1 = m - l + 1;
    uint16_t n2 =  r - m;

    /* create temp arrays */
    uint16_t L[n1], R[n2];

    /* Copy data to temp arrays L[] and R[] */
    for (i = 0; i < n1; i++)
        L[i] = arr[l + i];
    for (j = 0; j < n2; j++)
        R[j] = arr[m + 1+ j];

    /* Merge the temp arrays back into arr[l..r]*/
    i = 0; // Initial index of first subarray
    j = 0; // Initial index of second subarray
    k = l; // Initial index of merged subarray
    while (i < n1 && j < n2)
    {
        if (L[i] <= R[j])
        {
            arr[k] = L[i];
            i++;
        }
        else
        {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    /* Copy the remaining elements of L[], if there
       are any */
    while (i < n1)
    {
        arr[k] = L[i];
        i++;
        k++;
    }

    /* Copy the remaining elements of R[], if there
       are any */
    while (j < n2)
    {
        arr[k] = R[j];
        j++;
        k++;
    }
}

/* l is for left index and r is right index of the
   sub-array of arr to be sorted */
void mergeSort(uint16_t arr[], uint16_t l, uint16_t r)
{
    if (l < r)
    {
        // Same as (l+r)/2, but avoids overflow for
        // large l and h
        uint16_t m = l+(r-l)/2;

        // Sort first and second halves
        mergeSort(arr, l, m);
        mergeSort(arr, m+1, r);

        merge(arr, l, m, r);
    }
}


