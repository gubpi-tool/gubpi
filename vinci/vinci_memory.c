/****************************************************************************************/
/*                                                                                      */
/*                                    vinci_memory.c                                    */
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
/* some functions needed globally for the maintenance of the dynamic data structures    */
/*                                                                                      */
/****************************************************************************************/

#include "vinci.h"

/****************************************************************************************/
/*                                 memory allocation                                    */
/****************************************************************************************/

void *my_malloc (long int size)
   /* works exactly like malloc, but keeps trace of the used memory in the statistical  */
   /* variables and eventually frees the memory reserve                                 */

{  void *pointer;

   pointer = malloc (size);

   if (pointer == NULL)
   {
      fprintf (stderr, "\n***** ERROR: Out of memory\n");
      exit (0);
   }
   else
   {

   }

   return pointer;
}

/****************************************************************************************/

void *my_realloc (void *pointer, long int new_size, long int size_diff)
   /* works exactly like realloc, but keeps trace of the used memory in the statistical */
   /* variables and eventually frees the memory reserve                                 */
   /* size_diff is the difference between the new and the old size (usually positive    */
   /* for enlargements)                                                                 */

{
   pointer = realloc (pointer, new_size);

   if (pointer == NULL)
   {
      fprintf (stderr, "\n***** ERROR: Out of memory");
      exit (0);
   }
   else
   {

   }

   return pointer;
}

/****************************************************************************************/

void my_free (void *pointer, long int size)
   /* frees the memory space used by pointer and keeps track of the freed space in the  */
   /* statistical variables                                                             */

{
   free (pointer);

}



/****************************************************************************************/

void create_hyperplanes ()
   /* reserves memory space for the global variable G_Hyperplanes; G_m and G_d must be  */
   /* set correctly                                                                     */

{  int  i;

   G_Hyperplanes = (real **) my_malloc (G_m * sizeof (real *));

   for (i = 0; i < G_m; i++)
      G_Hyperplanes [i] = (real *) my_malloc ((G_d + 1) * sizeof (real));
         /* The last entry is needed for the right hand side. */

}

/****************************************************************************************/

void free_hyperplanes ()
   /* frees the memory space needed by the global variable G_Hyperplanes; G_m must be   */
   /* set correctly                                                                     */

{  int i;

   for (i = 0; i < G_m; i++)
      my_free (G_Hyperplanes [i], (G_d + 1) * sizeof (real));
   my_free (G_Hyperplanes, G_m * sizeof (real *));
}




/****************************************************************************************/

int *create_int_vector (int n)
   /* reserves memory space for a vector with n entries                                 */
   
{  int *v;

   v = (int *) my_malloc (n * sizeof (int));

   return v;
}

/****************************************************************************************/

void free_int_vector (int *v, int n)
   /* frees the memory space needed by the vector v of length n                         */

{  
   my_free (v, n * sizeof (int));
}

/****************************************************************************************/

rational **create_matrix (int m, int n)
   /* reserves memory space for an mXn matrix */
   
{  int     i;
   rational **A;
   
   A = (rational **) my_malloc (m * sizeof (rational *));
   for (i = 0; i < m; i++)
      A [i] = (rational *) my_malloc (n * sizeof (rational));
   
   return A;
}


/****************************************************************************************/

void free_matrix (rational **A, int m, int n)
   /* frees the memory space needed by the matrix A */

{  int i;

   for (i = 0; i < m; i++)
      my_free (A [i], n * sizeof (rational));
   my_free (A,  m * sizeof (rational *));
}

/****************************************************************************************/

void create_key (T_Key *key, int key_choice)
   /* creates the dynamic parts of the key; G_d must be set correctly */

{
   if (key_choice == KEY_PLANES_VAR)
   {
      key -> hypervar.hyperplanes =
                                 (T_LassInt *) my_malloc ((G_Storage + 2) * sizeof (T_LassInt));
      key -> hypervar.variables =
                                 (T_LassInt *) my_malloc ((G_Storage + 2) * sizeof (T_LassInt));
   }
}

/****************************************************************************************/

void free_key (T_Key key, int key_choice)
   /* frees the dynamic parts of the key */

{
   if (key_choice == KEY_PLANES_VAR)
   {
      my_free (key.hypervar.hyperplanes, (G_Storage + 2) * sizeof (T_LassInt));
      my_free (key.hypervar.variables, (G_Storage + 2) * sizeof (T_LassInt));
   }
}

/****************************************************************************************/
/*                               statistical routines                                   */
/****************************************************************************************/



/****************************************************************************************/
/*       routines for storing intermediate volumes in balanced trees (avl-trees)        */
/****************************************************************************************/

static int compare_key (T_Key key1, T_Key key2, int key_choice)
   /* compares key1 with key2; if key1 is smaller, -1 is returned, if key1 is bigger +1 */
   /* and if both are equal 0. key_choice determines which component of the key is      */
   /* relevant for comparing.                                                           */

{  int       i;
   T_LassInt *p1, *p2;

   switch (key_choice)
   {
   case KEY_VERTICES:
      if      (key1.vertices.d < key2.vertices.d) return -1;
      else if (key1.vertices.d > key2.vertices.d) return 1;
      else /* both volumes are for the same dimension */
      if      (key1.vertices.set.lastel < key2.vertices.set.lastel) return -1;
      else if (key1.vertices.set.lastel > key2.vertices.set.lastel) return 1;
      else
      {  /* both sets have the same cardinality */
         for (i=0; i <= key1.vertices.set.lastel; i++)
            if      (key1.vertices.set.loe [i] -> no < key2.vertices.set.loe [i] -> no)
               return -1;
            else if (key1.vertices.set.loe [i] -> no > key2.vertices.set.loe [i] -> no)
               return 1;
      }
      return 0;
      break;
   case KEY_PLANES_VAR:
/*
      for (i=0; ; i++)
         if      (key1.hypervar.hyperplanes [i] < key2.hypervar.hyperplanes [i])
                                                                        return -1;
	 else if (key1.hypervar.hyperplanes [i] > key2.hypervar.hyperplanes [i])
	                                                                return 1;
         else if (key1.hypervar.hyperplanes [i] > G_m)                  return 0;
*/
      for (p1 = key1.hypervar.hyperplanes, p2 = key2.hypervar.hyperplanes;;
           p1++, p2++)
         if      ((*p1) < (*p2)) return -1;
         else if ((*p1) > (*p2)) return  1;
         else if ((*p1) > G_m)   return  0;
      break;
   }

   /* to avoid warning */
   printf ("\n*** Control reaches place where it should not be.");
   exit (1);
}

/****************************************************************************************/

void tree_out (T_Tree **ppr , boolean *pi_balance, T_Key key, rational **volume,
   T_Key **keyfound, int key_choice)
   /* looks up the node corresponding to the variable "key" in the specified tree. If   */
   /* it is found the volume is returned via the variable of the same name.             */
   /* At the same time the found key is returned in "foundkey"; this is important for   */
   /* Lasserre, where only a part of the key is compared, but the whole key is needed   */
   /* later.                                                                            */
   /* Otherwise, a new node is created and a pointer to its volume part is returned via */
   /* the variable "volume" so that the computed volume can be inserted by the calling  */
   /* routine.                                                                          */
   /* As in the previous routine "key_choice" determines the active part of the keys.   */

{  T_Tree *p1, *p2;
   int	  cmp;

   /* Are we grounded? If so, add the node here and set the rebalance flag, then exit.  */
   if (!*ppr)
   {
      (*ppr) = (T_Tree *) my_malloc (sizeof (T_Tree));
      (*ppr) -> tree_l = NULL;
      (*ppr) -> tree_r = NULL;
      (*ppr) -> tree_b = 0;
      /* copy the key into the new node */
      create_key (&((*ppr) -> key), key_choice);
      switch (key_choice)
      {
      case KEY_VERTICES:
         (*ppr) -> key.vertices.set = duplicate_set (key.vertices.set);
         (*ppr) -> key.vertices.d = key.vertices.d;
         break;
      case KEY_PLANES_VAR:
         memcpy ((*ppr) -> key.hypervar.hyperplanes, key.hypervar.hyperplanes,
                 (G_Storage + 2) * sizeof (T_LassInt));
         memcpy ((*ppr) -> key.hypervar.variables, key.hypervar.variables,
                 (G_Storage + 2) * sizeof (T_LassInt));
         break;
      }
      (*ppr) -> vol = -1;       /* to recognise that element is newly created */
      *volume = &((*ppr) -> vol);
      *pi_balance = TRUE;
      return;
   }

   cmp = compare_key ((*ppr) -> key, key, key_choice);

   /* if LESS, prepare to move to the left. */
   if (cmp < 0)
   {
      tree_out (&((*ppr) -> tree_l), pi_balance, key, volume, keyfound, key_choice);
      if (*pi_balance)
      {  /* left branch has grown longer */
         switch ((*ppr) -> tree_b)
         {
         case 1:  /* right branch WAS longer; balance is ok now */
                  /* LESS: case 1.. balance restored implicitly */
            (*ppr) -> tree_b = 0;
            *pi_balance = FALSE;
            break;
         case 0:  /* balance WAS okay; now left branch longer */
                  /* LESS: case 0.. balance bad but still ok */
            (*ppr) -> tree_b = -1;
            break;
         case -1: /* left branch was already too long. rebalance */
            p1 = (*ppr) -> tree_l;
            if (p1 -> tree_b == -1)
            {  /* LESS: single LL */
               (*ppr) -> tree_l = p1->tree_r;
               p1 -> tree_r = (*ppr);
               (*ppr) -> tree_b = 0;
               (*ppr) = p1;
            }
            else
            {  /* LESS: real LR */
               p2 = p1 -> tree_r;
               p1 -> tree_r = p2 -> tree_l;
               p2 -> tree_l = p1;
               (*ppr) -> tree_l = p2 -> tree_r;
               p2 -> tree_r = (*ppr);
               if (p2 -> tree_b == -1)
                  (*ppr) -> tree_b = 1;
               else (*ppr) -> tree_b = 0;
               if (p2->tree_b == 1)
                  p1 -> tree_b = -1;
               else p1 -> tree_b = 0;
               (*ppr) = p2;
            }
            (*ppr) -> tree_b = 0;
            *pi_balance = FALSE;
         } /* switch */
      } /* if */
   } /* cmp < 0 */

   /* if MORE, prepare to move to the right. */
   else if (cmp > 0)
   {
      tree_out (&((*ppr) -> tree_r), pi_balance, key, volume, keyfound, key_choice);
      if (*pi_balance)
      {  /* right branch has grown longer */
         switch ((*ppr) -> tree_b)
         {
         case -1: /* MORE: balance was off, fixed implicitly */
            (*ppr) -> tree_b = 0;
            *pi_balance = FALSE;
            break;
         case 0:  /* MORE: balance was okay, now off but ok */
            (*ppr)->tree_b = 1;
            break;
         case 1:  /* MORE: balance was off, need to rebalance */
            p1 = (*ppr) -> tree_r;
            if (p1 -> tree_b == 1)
            {  /* MORE: single RR */
               (*ppr) -> tree_r = p1 -> tree_l;
               p1 -> tree_l = (*ppr);
               (*ppr) -> tree_b = 0;
               (*ppr) = p1;
            }
            else
            {  /* MORE: real RL */
               p2 = p1 -> tree_l;
               p1 -> tree_l = p2 -> tree_r;
               p2 -> tree_r = p1;
               (*ppr) -> tree_r = p2 -> tree_l;
               p2 -> tree_l = (*ppr);
               if (p2 -> tree_b == 1)
                  (*ppr) -> tree_b = -1;
               else (*ppr) -> tree_b = 0;
               if (p2 -> tree_b == -1)
                  p1 -> tree_b = 1;
               else p1 -> tree_b = 0;
               (*ppr) = p2;
            }
            (*ppr) -> tree_b = 0;
            *pi_balance = FALSE;
         } /* switch */
      } /* if */
   } /* cmp > 0 */

   /* not less, not more: this is the same key! give volume back! */
   else
   {
      *pi_balance = FALSE;
      *volume = &((*ppr) -> vol);
      *keyfound = &((*ppr) -> key);
   }
}

/****************************************************************************************/

void add_hypervar (T_LassInt hyperplane, T_LassInt variable, T_Key *key)
   /* adds the specified hyperplane and variable index to the variable "key" maintain-  */
   /* ing the ascending orders; if one index is G_m+1 resp. G_d+1 it is omitted.        */
   /* For the working of the procedure it is necessary that the last array entry is     */
   /* G_m + 1 resp. G_d + 1 and that there is still some space left in the arrays.      */
   /* Attention: Only use this function if you work with the planes and variables as    */
   /* key!                                                                              */

{  int i, j;

   if (hyperplane != G_m+1)
   {  
      for (i = 0; (*key).hypervar.hyperplanes [i] < hyperplane; i++);
      if ((*key).hypervar.hyperplanes [i] != hyperplane)
      {  /* insert index */
         for (j = G_d; j > i; j--)
            (*key).hypervar.hyperplanes [j] = (*key).hypervar.hyperplanes [j-1];
         (*key).hypervar.hyperplanes [i] = hyperplane;
      }
   }

   if (variable != G_d+1)
   {  
      for (i = 0; (*key).hypervar.variables [i] < variable; i++);
      if ((*key).hypervar.variables [i] != variable)
      {  /* insert index */
         for (j = G_d; j > i; j--)
            (*key).hypervar.variables [j] = (*key).hypervar.variables [j-1];
         (*key).hypervar.variables [i] = variable;
      }
   }
}


/****************************************************************************************/

void delete_hypervar (T_LassInt hyperplane, T_LassInt variable, T_Key *key)
   /* deletes the indices from the variable key; if one index is -1 it is omitted.      */
   /* Attention: Only use this function if you work with the planes and variables as    */
   /* key!                                                                              */
   
{  int i, j;

   if (hyperplane != G_m+1)
   {  
      for (i = 0; i <= G_d && (*key).hypervar.hyperplanes [i] != hyperplane; i++);
      if (i != G_d + 1)
      {  /* index found, delete it */
         for (j = i; (*key).hypervar.hyperplanes [j] != G_m + 1; j++)
            (*key).hypervar.hyperplanes [j] = (*key).hypervar.hyperplanes [j+1];
      }
   }

   if (variable != G_d+1)
   {  
      for (i = 0; i <= G_d && (*key).hypervar.variables [i] != variable; i++);
      if (i != G_d + 1)
      {  /* index found, delete it */
         for (j = i; (*key).hypervar.variables [j] != G_d + 1; j++)
            (*key).hypervar.variables [j] = (*key).hypervar.variables [j+1];
      }
   }
}

/****************************************************************************************/
