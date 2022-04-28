/****************************************************************************************/
/*                                                                                      */
/*                                   vinci_set.c                                        */
/*                                                                                      */
/****************************************************************************************/
/*                                                                                      */
/* Authors: Benno Bueeler (bueeler@ifor.math.ethz.ch)                                   */
/*          and                                                                         */
/*          Andreas Enge (enge@ifor.math.ethz.ch)                                       */
/*          Institute for Operations Research                                           */
/*	    Swiss Federal Institute of Technology Zurich                                     */
/*	    Switzerland                                                                      */
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
/*  functions on the data structures "set of pointers to vertices" and sets of them     */
/*                                                                                      */
/****************************************************************************************/

#include "vinci.h"

/****************************************************************************************/
/*                           functions on sets of vertices                              */
/****************************************************************************************/


/****************************************************************************************/

T_VertexSet duplicate_set (T_VertexSet s)
   /* creates a new set with the same elements as s */
   
{  T_VertexSet newset;

   newset.maxel = s.lastel;
   newset.lastel = s.lastel;
   newset.loe = (T_Vertex **) my_malloc ((s.lastel + 1) * sizeof (T_Vertex *));
   memcpy (newset.loe, s.loe, (s.lastel + 1) * sizeof (T_Vertex *));
   return newset;
}
   
/****************************************************************************************/
