#pragma once

/****************************************************************************************/
/*                                                                                      */
/*                                      vinci.h                                         */
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
/* global constants, types, variable and function declarations                          */
/*                                                                                      */
/****************************************************************************************/




/****************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


/****************************************************************************************/
/* The following constants may be changed by a user                                     */
/****************************************************************************************/


#define PIVOTING 1
#define MIN_PIVOT 0.5
#define PIVOTING_LASS 0
#define MIN_PIVOT_LASS 0.1
   /* for choosing a pivoting strategy whenever this is needed, e.g. for determinant    */
   /* computation. The last two constants are valid for Lasserre's method, the first    */
   /* two for any other method. PIVOTING and PIVOTING_LASS can take the following       */
   /* values:                                                                           */
   /*  0: The first row with entry bigger than MIN_PIVOT (in its absolute value) is     */
   /*     chosen.                                                                       */
   /*  1: partial pivoting; the row with maximal entry (absolute value) is chosen;      */
   /*     MIN_PIVOT is ignored.                                                         */
   /*  2: total pivoting; the row and column with maximal entry (absolute value) are    */
   /*     chosen, MIN_PIVOT is ignored. If total pivoting is not possible (e. g. for    */
   /*     solving linear equations) partial pivoting is performed.                      */

#define DEFAULT_STORAGE 20
   /* For so many recursion levels intermediate volumes are stored, starting from the   */
   /* level with two planes fixed. The value is only active if no option -s is speci-   */
   /* fied; it may be set to 0, for instance, by using the option -s0.                  */




/****************************************************************************************/
/* The following constants should not be changed by a user                              */
/****************************************************************************************/

#define EPSILON 1e-10
 

                          /* times() */

#define FALSE 0
#define TRUE 1

#define NONE       0      /* constants for data types in files */
#define INTEGER_T  1
#define REAL_T     2
#define RATIONAL_T 3


#define KEY_VERTICES   2  /* constants for the key type actually used in the balanced   */
#define KEY_PLANES_VAR 3  /* tree routines                                              */


   /* The simplices in the size classes from 1e(STAT_SMALLEST_EXP) to                   */
   /* 1e(STAT_BIGGEST_EXP) are counted separately for each class. Simplices too small   */
   /* or too big are summarized in one variable. */


/****************************************************************************************/
/*                                  type definitions                                    */
/****************************************************************************************/

typedef double        real;
typedef unsigned char boolean;
typedef unsigned char T_LassInt;

#ifdef RATIONAL
   typedef Rational rational;
#else
   typedef real     rational;
#endif

struct T_Vertex
     {real *coords;
          /* the coordinates of a vertex */
      int no;
     }; /* type of a vertex */
typedef struct T_Vertex T_Vertex;

/* The sets of vertices are implemented as ordered lists */
struct T_VertexSet
       {int maxel;      /* The list of vertices loe may contain elements 0 to maxel   */
        int lastel;     /* Effectively, loe contains elements from 0 to lastel;       */
                        /* an empty set is indicated by lastel == -1.                 */
        T_Vertex **loe; /* The elements of the set are pointers to vertices which are */
                        /* stored in loe, in ascending order following the numbering  */
                        /* of the vertices */
       };
typedef struct T_VertexSet T_VertexSet;


/* The sets of VertexSets are implemented as linked chain because only simple           */
/* operations are needed on them. */
struct T_VertexSuperset
       {struct T_VertexSuperset *next;
        struct T_VertexSet       content;
       };
typedef struct T_VertexSuperset T_VertexSuperset;


/* Types for storing face volumes in balanced trees. For storing and retrieving a key   */
/* is needed for every face; all possibilities for keys are defined by the union T_Key. */
/* Only one of the keys is active at a time. T_Tree defines the tree itself.            */
union T_Key
      {
       struct {T_VertexSet set;
               int         d;
              } vertices;
          /* The vertices contained in the face and the dimension for which the volume  */
          /* is stored.                                                                 */
       struct {T_LassInt *hyperplanes, *variables;} hypervar;
          /* A list of fixed constraints and variables onto which the face has been     */
          /* projected, used for Lasserre's formula                                     */
      };
typedef union T_Key T_Key;

struct T_Tree
       {struct T_Tree *tree_l, *tree_r; /* the left and right subtrees */
        int           tree_b;
        T_Key         key;
        rational      vol;              /* the stored volume */
       };
typedef struct T_Tree T_Tree;

/****************************************************************************************/
/*                            global variable declarations                              */
/****************************************************************************************/

extern int G_d;
extern int G_m;
extern int G_n;
extern real **G_Hyperplanes;
   /* The first d components are the coordinates of a normal vector, the d+first com-   */
   /* ponent is the right hand side. */
                                              
extern int G_Storage;
   /* see the annotations for DEFAULT_STORAGE                                           */




/****************************************************************************************/
/*                    functions and procedures from 'vinci_memory'                      */
/****************************************************************************************/

void *my_malloc (long int size);
void *my_realloc (void *pointer, long int new_size, long int size_diff);
void my_free (void *pointer, long int size);
void create_hyperplanes ();
void free_hyperplanes ();
int *create_int_vector (int n);
void free_int_vector (int *v, int n);
rational **create_matrix (int m, int n);
void free_matrix (rational **A, int m, int n);
void create_key (T_Key *key, int key_choice);
void free_key (T_Key key, int key_choice);

void tree_out (T_Tree **ppr , boolean *pi_balance, T_Key key, rational **volume,
   T_Key **keyfound, int key_choice);
void add_hypervar (T_LassInt hyperplane, T_LassInt variable, T_Key *key);
void delete_hypervar (T_LassInt hyperplane, T_LassInt variable, T_Key *key);

/****************************************************************************************/
/*                      functions and procedures from 'vinci_set'                       */
/****************************************************************************************/

T_VertexSet duplicate_set (T_VertexSet s);



/****************************************************************************************/
/*                 functions and procedures from 'vinci_lass'                           */
/****************************************************************************************/

void volume_lasserre_file (rational *volume, const char *input, int length);



/****************************************************************************************/
/*                   functions and procedures from 'vinci_file'                         */
/****************************************************************************************/

void read_hyperplanes (const char *input, int length);

/****************************************************************************************/
