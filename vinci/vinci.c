/****************************************************************************************/
/*                                                                                      */
/* Last Changes: April 4, 2021                                                          */
/*                                                                                      */
/****************************************************************************************/

/****************************************************************************************/
/* This is a modifed version of the tool VINCI originall developed by Benno Bueeler     */
/* and Andreas Enge. The original authors are named in all files that were modifed.     */
/* The main changes are concered with simplifying the output format of                  */
/* the tool and removing not-needed features.                                           */ 
/****************************************************************************************/
#include "vinci.h"

int main (int argc, char *argv [])
{  
   rational volume;
   size_t len;

   if(argc < 2) {

      printf ("Must supply argument");
      return 0;
   }

   len = strlen(argv[1]);

   volume_lasserre_file (&volume, argv[1], len);

   printf ("%f", volume);

   return 0;
}

/****************************************************************************************/
