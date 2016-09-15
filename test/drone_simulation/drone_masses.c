/*
 * drone_masses.c
 *
 *  Created on: May 13, 2016
 *      Author: jakov
 */

#include "physical_definitions.h"
#include "drone_masses.h"

/* Simulation with four inner masses,
 * four outer masses
 * and four propellers
 */

Masspoint_t drone_masspoints[NUMBER_OF_MASSPOINTS] = {
		{
				.v.v = {
						INNER_MASS_RADIUS,
						0,
						0,
				},
				.m = INNER_MASS,
		},
		{
				.v.v = {
						-INNER_MASS_RADIUS,
						0,
						0,
				},
				.m = INNER_MASS,
		},
		{
				.v.v = {
						0,
						INNER_MASS_RADIUS,
						0,
				},
				.m = INNER_MASS,
		},
		{
				.v.v = {
						0,
						-INNER_MASS_RADIUS,
						0,
				},
				.m = INNER_MASS,
		},
		{
				.v.v = {
						OUTER_MASS_RADIUS,
						0,
						0,
				},
				.m = OUTER_MASS,
		},
		{
				.v.v = {
						-OUTER_MASS_RADIUS,
						0,
						0,
				},
				.m = OUTER_MASS,
		},
		{
				.v.v = {
						0,
						OUTER_MASS_RADIUS,
						0,
				},
				.m = OUTER_MASS,
		},
		{
				.v.v = {
						0,
						-OUTER_MASS_RADIUS,
						0,
				},
				.m = OUTER_MASS,
		},

};

