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

const Masspoint_t drone_masspoints[8] = {
		{
				.v = {
						.x = INNER_MASS_RADIUS,
						.y = 0,
						.z = 0,
				},
				.m = INNER_MASS,
		},
		{
				.v = {
						.x = -INNER_MASS_RADIUS,
						.y = 0,
						.z = 0,
				},
				.m = INNER_MASS,
		},
		{
				.v = {
						.x = 0,
						.y = INNER_MASS_RADIUS,
						.z = 0,
				},
				.m = INNER_MASS,
		},
		{
				.v = {
						.x = 0,
						.y = -INNER_MASS_RADIUS,
						.z = 0,
				},
				.m = INNER_MASS,
		},
		{
				.v = {
						.x = OUTER_MASS_RADIUS,
						.y = 0,
						.z = 0,
				},
				.m = OUTER_MASS,
		},
		{
				.v = {
						.x = -OUTER_MASS_RADIUS,
						.y = 0,
						.z = 0,
				},
				.m = OUTER_MASS,
		},
		{
				.v = {
						.x = 0,
						.y = OUTER_MASS_RADIUS,
						.z = 0,
				},
				.m = OUTER_MASS,
		},
		{
				.v = {
						.x = 0,
						.y = -OUTER_MASS_RADIUS,
						.z = 0,
				},
				.m = OUTER_MASS,
		},

};

