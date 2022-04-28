/****************************************************************************************/
/*                                                                                      */
/*                                      vinci_global.c                                  */
/*                                                                                      */
/****************************************************************************************/
/*                                                                                      */
/* Authors: Benno Bueeler (bueeler@ifor.math.ethz.ch)                                   */
/*          and                                                                         */
/*          Andreas Enge (enge@ifor.math.ethz.ch)                                       */
/*          Institute for Operations Research                                           */
/*	    Swiss Federal Institute of Technology Zurich                                    */
/*	    Switzerland                                                                     */
/*                                                                                      */
/* Last Changes: February 4, 2001                                                       */
/*                                                                                      */
/****************************************************************************************/
/*                                                                                      */
/* Modifications: This file, originally due to Benno Bueeler and Andreas Enge, is       */
/* modified.                                                                            */
/* Date of the last modification: April, 2021                                           */
/*                                                                                      */
/****************************************************************************************/
/*                                                                                      */
/* definitions of the global variables                                                  */
/*                                                                                      */
/****************************************************************************************/

#include "vinci.h"

/****************************************************************************************/

int G_d = 0;
int G_m = 0;
int G_n = 0;

real **G_Hyperplanes = NULL;

int G_Storage = -1;



/****************************************************************************************/
