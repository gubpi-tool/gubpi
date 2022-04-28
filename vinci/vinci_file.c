/****************************************************************************************/
/*                                                                                      */
/*                                    vinci_file.c                                      */
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
/* Last Changes: May 19, 2003                                                           */
/*                                                                                      */
/****************************************************************************************/
/*                                                                                      */
/* Modifications: This file, originally due to Benno Bueeler and Andreas Enge, is       */
/* modified.                                                                            */
/* Date of the last modification: April, 2021                                           */
/*                                                                                      */
/****************************************************************************************/
/*                                                                                      */
/* functions allowing to read data files in the format of Avis and Fukuda               */
/*                                                                                      */
/****************************************************************************************/

#include "vinci.h"


  

/****************************************************************************************/

void read_hyperplanes (const char *input, int length)
   /* reads the hyperplanes from the specified file to the global variable              */
   /* G_Hyperplanes and sets G_m and G_d                                                */
   /* The file must contain the hyperplanes in the following polyhedra format of Avis   */
   /* and Fukuda:                                                                       */
   /* comments                                                                          */
   /* begin                                                                             */
   /* number of hyperplanes m  dimension + 1   type of coordinates                      */
   /* b   -A                                                                            */
   /* end or any other text (is ignored)                                                */

{  
   char* puffer = (char*)malloc(sizeof(char) * length);

   int i = 0;
   int ri = 0;
   int rj = 0;

   /* Read G_m */
   while(i <= length){
      if(input[i] == ' ' || input[i] == '\n') {
         i++;
      } else {
         /* Find string up to next blank */
         int j = i;
         while(j <= length){
            if(input[j] == ' ' || input[j] == '\n') {
               break;
            }
            j++;
         }

         puffer = strncpy(puffer, input + i, j-i);
         puffer[j-i] = '\0';

         sscanf(puffer, "%i", &G_m);

         i = j;
         break;
      }
   }

   /* Read G_d */
   while(i <= length){
      if(input[i] == ' ' || input[i] == '\n') {
         i++;
      } else {
         /* Find string up to next blank */
         int j = i;
         while(j <= length){
            if(input[j] == ' ' || input[j] == '\n') {
               break;
            }
            j++;
         }

         puffer = strncpy(puffer, input + i, j-i);
         puffer[j-i] = '\0';

         sscanf(puffer, "%i", &G_d);

         i = j;
         break;
      }
   }

   G_d --;

   create_hyperplanes ();

   

   /* Read indices */
   while(i <= length){
      if(input[i] == ' ' || input[i] == '\n') {
         i++;
      } else {
         int j = i;
         while(j <= length){
            if(input[j] == ' ' || input[j] == '\n') {
               break;
            }
            j++;
         }

         puffer = strncpy(puffer, input + i, j-i);
         puffer[j-i] = '\0';

         i = j;


         if(rj == 0){
            sscanf(puffer, "%le", &(G_Hyperplanes [ri] [G_d]));
         } else {
            sscanf(puffer, "%le", &(G_Hyperplanes [ri] [rj-1])); /* -1 to adjust fro manual offset */
            G_Hyperplanes [ri] [rj-1] = - G_Hyperplanes [ri] [rj-1];
         }

         rj++;

         if(rj == G_d + 1){
            rj = 0;
            ri++;

            if(ri == G_m){
               break;
            }
         }

         
      }
   }
}

/****************************************************************************************/
