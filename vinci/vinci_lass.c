/****************************************************************************************/
/*                                                                                      */
/*                              vinci_lass.c                                            */
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
/* Last Changes: July 6, 2003                                                           */
/*                                                                                      */
/****************************************************************************************/
/*                                                                                      */
/* Modifications: This file, originally due to Benno Bueeler and Andreas Enge, is       */
/* modified.                                                                            */
/* Date of the last modification: April, 2021                                           */
/*                                                                                      */
/****************************************************************************************/
/*                                                                                      */
/* Lasserre's volume computation method                                                 */
/*                                                                                      */
/****************************************************************************************/

#include "vinci.h"


#define MAXIMUM 1.0e150   /* define the maximum used for testing infinity */
#define	EPSILON_LASS EPSILON   /* Numbers smaller than this are treated as zero in rhs*/
#define	EPS1    EPSILON    /* Numbers smaller than this are treated as zero in coefficient */
#define	EPS_NORM EPSILON   /* EPSILON used for constraint normalization*/
#define LaShiftLevel 0    /* Shifting is possible if d>=LaShiftLevel */
#define LaShift 1         /* shift polytope to make at least d components of
                             the rhs zero if there are less than d-LaShift zeros */

/******************/
/*global variables*/
/******************/

rational *A;
rational *pivotrow;             /* copy of pivot row */
T_LassInt *All_index;  /* All eliminated and superfluous indices (sorted) */
T_LassInt *Pivot;      /* All substituted variables (sorted) */
int **p2c;        /* pivot to constraints: which variable is fixed in which constraint;
                     the variable index is given in the leading column, the constraint 
		     index in the second */

rational  * planescopy;               /* needed in shift_P in the lasserre-code */
static T_Key   key, *keyfound;    /* key for storing the actually considered face and   */
                                  /* found key when a volume could be retrieved */
static T_Tree  *tree_volumes;     /* tree for storing intermediate volumes */


/***************/
/*help routines*/
/***************/


static T_LassInt add_reduced_index(T_LassInt red, T_LassInt * indices, 
                                 T_LassInt * ref_indices)
/* insert new index into ref_indices maintaining sorting; if indices!=NULL this index
   is also inserted into indices. returns the original index base.
   assumption: red counted in the original systen is not contained in ref_indices */

{ register int i;
  T_LassInt xch, base;
    
  for (i=0; red>=ref_indices[i]; i++) red++;  /* reduced index -> original index */
  base=red;  
  while (ref_indices[i]<=G_m) {
      xch=ref_indices[i];
      ref_indices[i]=red;
      red=xch;
      i++;
  };
  ref_indices[i]=red;
  ref_indices[i+1]=G_m+2;
  if (indices==NULL) return base;
  red=base;
  for (i=0; base>indices[i]; i++);
  while (indices[i]<=G_m) {
      xch=indices[i];
      indices[i]=red;
      red=xch;
      i++;
  };
  indices[i]=red;
  indices[i+1]=G_m+2;
  return base;
}


static void del_original_indices(T_LassInt *indices, T_LassInt *org_indices)
/* delete original indices in org_indices maintaining sorting.
   assumption: all the indices are contained in org_indices.
   the end of indices is marked by G_m+2 */

{   register int i, cnt;

    i=cnt=0;
    while (org_indices[i]<=G_m) {
	while ((org_indices[cnt+i]==indices[cnt])&&(indices[cnt]<=G_m)) cnt++;
	org_indices[i]=org_indices[i+cnt];
	i++;
    }
}


static void del_original(T_LassInt base, T_LassInt * indices)
/* delete base in indices maintaining sorting.
   assumption: base is contained in indices. */

{ int i;

  for (i=0; ((base!=indices[i])&&(indices[i]<=G_m)); i++);  /* search original index */
  if (base!=indices[i]) {
      fprintf(stderr, "ERROR: Deletion index not found!\n");
      exit(0);
  };
  for (;indices[i]<=G_m;i++) indices[i]=indices[i+1];
}  


static void rm_original_inElAll_index(T_LassInt baserow)
/* delete baserow in All_index maintaining sorting. */
{   del_original(baserow, All_index); }


static void rm_constraint(rational* A, int *LastPlane_, int d, int rm_index)
/* removes the constraints given in rm_index and adjusts *LastPlane */

{   register rational *p1, *p2; 
    register int i;

    p1=A+rm_index*(d+1);
    p2=A+(rm_index+1)*(d+1);
    for (i=0; i<(((*LastPlane_)-rm_index)*(d+1)); i++) {
	*p1=*p2;
	p1++;
	p2++;
    };
    (*LastPlane_)--;
}


static rational * compact()
{   register int i, j;
    register rational *po,*pc;

    if (!(pc = (rational *) my_malloc (G_m*(G_d+1)*sizeof(rational)))) {
	fprintf (stderr, "\n***** ERROR: Out of memory in 'compact.*pc'");
	exit(0); 
    }
    po=pc;
    for (i=0; i<G_m; i++) {
	for (j=0; j<=G_d; j++,pc++) *pc= G_Hyperplanes [i][j];
    };
    return po;
}


/***************/
/*Core routines*/
/***************/


static int notInPivot(int * pivot, int col, int i)
{ register int h;
  for (h=0;h<col;h++)
   if (pivot[h]==i) return FALSE;
  return TRUE;
}


static void shift_P(rational *A, int LastPlane_, int d)
/*  shift one vertex of the polytope into the origin, that
    is, make at least d components of b equal zero */

{   register rational  *p1, *p2, *p3, d1, d2, d3;
    register int col, i, j;
    static int *pivot = NULL;
                 /* contains the pivot row of each column */

    

    if (pivot == NULL) pivot = create_int_vector (G_d + 1);
    
    p1=A;                         /* search pivot of first column */
    pivot[0]=0; 
    d3=fabs(d1=*p1);
    for (i=0; i<=LastPlane_; i++) {
        d2=fabs(*p1);
#if PIVOTING_LASS == 0
	if (d2>=MIN_PIVOT_LASS) {pivot[0]=i; d1=*p1; break;};
#endif
	if (d2>d3) { pivot[0]=i; d1=*p1; d3=d2; };
	p1+=(d+1);
    }
    /* copy pivot row into planescopy */
    p1=A+pivot[0]*(d+1)+1;   
    p2=planescopy+pivot[0]*(d+1)+1;
    for (i=1,d2=1.0/d1; i<=d; i++,p1++,p2++) *p2 = (*p1)*d2;
    /* complete first pivoting and copying */
    p1=A+1;                          
    p2=planescopy+1;
    for (i=0; i<=LastPlane_; i++, p1++, p2++) {
	if (i==pivot[0]) {
	    p1+=d;
	    p2+=d;
	    continue;   /* pivot row already done */
	}
	d1=*(p1-1); 
	p3=planescopy+pivot[0]*(d+1)+1;
	for (j=1; j<=d; j++, p1++, p2++, p3++) (*p2)=(*p1)-d1*(*p3);
    }
    
    /* subsequent elimination below */
  
    for (col=1;col<d;col++) {
	for (i=0;i<=LastPlane_;i++)       /* search first row not already used as pivot row*/
	    if (notInPivot(pivot,col,i)) {
		pivot[col]=i; 
		break; 
	    }
	p1=planescopy+i*(d+1)+col;               /* search subsequent pivot row */
	d3=fabs(d1=*p1);
	for (; i<=LastPlane_; i++, p1+=(d+1))  
	    if (notInPivot(pivot,col,i)) {
	        d2=fabs(*(p1));
#if PIVOTING_LASS == 0
		if (d2>=MIN_PIVOT_LASS) {
		    pivot[col]=i; 
		    d1=*p1;
		    break; 
		}
#endif
		if (d2>d3) {
		    pivot[col]=i;
		    d1=*p1;
		    d3=d2;
		}
	    };
	/* update pivot row */
	p1=planescopy+pivot[col]*(d+1)+col+1;
	d2=1.0/d1;
	for (j=col+1; j<=d; j++, p1++) (*p1) *= d2;
	if (col==(d-1)) break;   /* the rest is not needed in the last case */
        /* update rest of rows */
        p1=planescopy+col+1;
        p2=planescopy+pivot[col]*(d+1)+col+1;
	for (i=0; i<=LastPlane_; i++, p1+=(col+1)) {
	    if (!notInPivot(pivot,col+1,i)) {
	        p1+=d-col;
		continue;
	    }
	    d1=*(p1-1);
	    for (j=col+1; j<=d; j++, p1++, p2++) *p1=(*p1)-d1*(*p2);
	    p2-=d-col;
	}
    };

    /* compute x* by backward substitution; result goes into rhs of planescopy */

    for (i=d-2; 0<=i; i--){
        p1=planescopy+pivot[i]*(d+1)+d;
	p2=p1-d+i+1;
	for (j=i+1; j<d; j++, p2++)
	    *(p1)-= (*p2)*(*(planescopy+pivot[j]*(d+1)+d));
    }
 
    /* compute shifted b  */

    for (i=0; i<=LastPlane_; i++) {
        p1=A+i*(d+1);
        p2=p1+d;
	if (notInPivot(pivot,d,i)) 
	    for (j=0; j<d; j++,p1++) {
		*p2 -= (*p1)*(*(planescopy+pivot[j]*(d+1)+d));
	    }
	else *p2=0;
    }
}

static int norm_and_clean_constraints(rational* A, int *LastPlane_, int d, 
                               T_LassInt *Del_index, int Index_needed)
/* Other (simpler) implementation of version lasserre-v15.
   Finally (up to the sign) identical constraints in A are detected. If they are
   identical the back one is removed, otherwise the system is infeasible. LastPlane_
   is reduced accordingly to the elimination process as well as insertion of the
   corresponding original indices into Del_index if Index_needed is true. */

{   register int i, j, row = 0;
    register rational r0, *p1, *p2;

    /* find nonzero[][] and maximal elements and normalize */
  
    p1=A;                                  /* begin of first constraint */
    while (row<=(*LastPlane_)) {           /* remove zeros and normalize */
	r0=0.0;                            /* norm of vector */
        for (j=0; j<d; j++,p1++) 
	    r0+=(*p1)*(*p1);               /* compute euclidean norm */
        r0=sqrt(r0);
	if (r0<EPS_NORM) {
            if ((*p1)<-100000*EPS1){      /* if negative rhs */
		return 1;                  /* infeasible constraint */
	    }
	    rm_constraint(A, LastPlane_, d,row);
	    if (Index_needed) add_reduced_index(row, Del_index, All_index);
	    p1-=d;
	}
	else {
	    r0=1.0/r0;
	    p1-=d;
	    for (j=0; j<=d; j++,p1++)
		(*p1)*=r0;
	    row++; 
	}
    }

    /* detect identical or reverse constraints */
    
    for (row=0; row<(*LastPlane_); row++) {
	i=row+1;
	while (i<=*LastPlane_) {        /* test all subsequent rows i if equal to row */
            r0=0.0;
 	    p1=A+row*(d+1);
	    p2=A+i*(d+1);
            for (j=0;j<d;j++,p1++,p2++)
	        r0+=(*p1)*(*p2);        /* cosinus of arc among those two vectors */
	    if (r0>0) {
	        /* NEW VERSION of removing constraints */ 
	        if (fabs(r0-1.0)<EPS_NORM) {
		    if ((*p1)>(*p2)){
		    	if (Index_needed) add_reduced_index(row, Del_index, All_index);
			rm_constraint(A, LastPlane_, d,row);
			i=row+1;
                    }
		    else {
			if (Index_needed) add_reduced_index(i, Del_index, All_index);
			if (i<(*LastPlane_)) 
			    rm_constraint(A, LastPlane_, d,i);
			else (*LastPlane_)--;
                    }
		}
                else i++;

                /* OLD VERSION :
	        if ((fabs(r0-1.0)<EPS_NORM) && (fabs((*p1)-(*p2))<EPS1)){
		    if (Index_needed) add_reduced_index(i, Del_index, All_index);
		    if (i<(*LastPlane_)) 
			rm_constraint(A, LastPlane_, d,i);
		    else (*LastPlane_)--;
                }
                else i++;
		*/
	    }
	    else {
	        if (fabs(r0+1.0)<EPS_NORM){
		    if ((*p1)>0){
		        if ((*p2)<(EPS1-(*p1))) return 1; 
		     }
		     else {
		         if ((*p1)<(EPS1-(*p2))) return 1; 
		     }
		}
	        i++;
	    }
	}
    }
    return 0;  /* elimination succesful */
}


rational scale(int dimdiff, T_LassInt *fvTree, T_LassInt * fvNew)
{   int i, j, k, l, m, n;
    int *pcol,  /* pivot columns */
        *dcol,  /* determinant columns */ 
        *frow;  /* fixed constraints (rows) */
    rational **Ascale; /* contains complete rows of scaling matrix */
    rational **Adet; /* contains square scaling matrix */
    rational r1;

   pcol = create_int_vector (dimdiff);
   dcol = create_int_vector (dimdiff);
   frow = create_int_vector (dimdiff);
	Ascale = create_matrix (dimdiff, G_d);
	Adet = create_matrix (dimdiff, dimdiff);

    /* extract index sets where projection on differing subspaces happened */

    j=l=m=n=0; 
    for (i=0; i<G_d; i++){
        while (fvTree[j]<i) j++;
	if (fvTree[j]>i) { /* i is not in fvTree */
	    for (k=0; k<dimdiff; k++){
	        if (i==p2c[k][0]){
		    pcol[l]=i;
		    frow[l]=k;   /* =p2c[k][1]; */
		    l++;
		    break;
	        }
	    }
	}
	else { /* i is in fvTree */
            while (fvNew[m]<i) m++;
	    if (fvNew[m]>i) { /* i is not in fvNew */
	        dcol[n]=i;
                n++;
	    }
	}
    }

    /* test consistent dimensionality; can be omitted */

    if (n!=l) {
        fprintf(stderr, "\n***** ERROR: Non-square scaling matrix in 'scale'\n");
	exit(1);
    };

    if (n==0) return 1; /* projection was done on the same subspace */ 

    /* Build Ascale and inverte right half by left half */

    for (i=0; i<dimdiff; i++)
        for (j=0; j<G_d; j++) Ascale[i][j] = G_Hyperplanes[p2c[i][1]][j];
    for (i=0; i<dimdiff; i++){
	r1=1/Ascale[i][p2c[i][0]];
	for (j=0; j<G_d; j++) Ascale[i][j]*=r1;  /* divide pivot-row by pivot-element */
        for (j=0; j<dimdiff; j++){
	    if (i==j) continue;
	    for (k=0; k<G_d; k++){
	        if (k!=p2c[i][0])
	            Ascale[j][k] -= Ascale[i][k]*Ascale[j][p2c[i][0]];
	    }
	}
    }

    /* extract determinant submatrix Adet */

    for (i=0; i<n; i++)
        for (j=0; j<n; j++)
            Adet[i][j]=Ascale[frow[i]][dcol[j]];
    
    if (n==1) { /* here the determinant is trivial */
        return 1/fabs(Adet[0][0]);
    }

    /* compute determinant of Adet (modulo sign due to permutation) */

    for (i=0; i<n; i++) pcol[i]=-1;
    for (i=0; i<n-1; i++){
        j=0;                     /* search for pivot column */
	while (pcol[j]>=0) j++;
	for (k=j+1; k<n; k++) { 
	    if (pcol[k]>=0) continue;
	    if (fabs(Adet[i][k])>fabs(Adet[i][j])) j=k;
	};
        pcol[j]=i;
	for (k=i+1; k<n; k++)
	    for (l=0; l<n; l++){
	        if (l!=j)
                    Adet[k][l] -= Adet[i][l]/Adet[i][j]*Adet[k][j];
	    }
    };
    
    r1=1;
    for (i=0; i<n; i++) {
        if (pcol[i]>=0) r1*=Adet[pcol[i]][i];
	else r1*=Adet[n-1][i];
    }
	 
	 free_int_vector (pcol, dimdiff);
	 free_int_vector (dcol, dimdiff);
	 free_int_vector (frow, dimdiff);
	 free_matrix (Ascale, dimdiff, G_d);
	 free_matrix (Adet, dimdiff, dimdiff);

    return 1/fabs(r1);
}


static rational lass(rational *A, int LastPlane_, int d)
/* A has exact dimension (LastPlane_+1)*(d+1). The function returns
   the volume; an underscore is appended to LastPlane_ and d */

{   rational * redA;            /* A reduced by one dimension and constraint */
    int i, j;
    T_LassInt baserow = 0, basecol = 0, col;
    int dimdiff, row;         /* dimension difference */
    boolean store_volume;
    boolean i_balance = FALSE;
    rational ma, mi, *volume, *realp1, *realp2;
    int Index_needed;         /* Boolean, if index operations are needed */
    T_LassInt * Del_index = NULL; /* contains the indices of the deleted planes */

    /* test if volume is already known and return it if so */

    dimdiff = G_d-d;
    if ((G_Storage > (dimdiff-2)) && (dimdiff >= 2)) {
        tree_out (&tree_volumes, &i_balance, key, &volume, &keyfound, KEY_PLANES_VAR);
        if ((*volume)>=0)  {  /* this volume has already been computed */
	    
	    return (*volume)*scale(dimdiff, 
	                           keyfound->hypervar.variables,
				   key.hypervar.variables);
	}
        (*volume)=0;      /* initialize */
        store_volume=TRUE;
       
    }
    else store_volume=FALSE;

    /* if d==1 compute the volume and give it back */

    if (d == 1) {
	ma=-MAXIMUM;
	mi= MAXIMUM;
	for (i=0; i<=LastPlane_; i++,A+=2) { 
	    if (*A>EPSILON_LASS) { if ((*(A+1)/ *A)<mi) mi=(*(A+1)/ *A); }
	    else if (*A<-EPSILON_LASS) { if ((*(A+1)/ *A)>ma) ma=*(A+1)/ *A; } 
            else if ((*(A+1))<-(100000*EPSILON_LASS)) return 0; 
	}
	if ((ma<-.5*MAXIMUM)||(mi>.5*MAXIMUM)) {
	    printf("\nVolume is unbounded!\n");
	    exit(0);
	}
	if ((mi-ma)>EPSILON_LASS) {
	    if (store_volume) (*volume)=mi-ma;
	    return mi-ma;
	}
	return 0;
    }

    /* if d>1 apply the recursive scheme by fixing constraints. */

    Index_needed = (G_Storage>(G_d-d-1));
    if (Index_needed){
	if (!(Del_index = (T_LassInt *) my_malloc ((LastPlane_ + 2) * sizeof (T_LassInt)))){
	    fprintf (stderr, "\n***** ERROR/WARNING: Out of memory in 'lass'\n");
	    exit(0);
	};
        Del_index[0]=G_m+2;   /* initialize: mark end */
    }
    ma=0;                                         /* used to sum up the summands */
    if (norm_and_clean_constraints(A, &LastPlane_, d, Del_index, Index_needed)!=0)
        goto label2;

    /* if appropriate shift polytope */

    if (d>=LaShiftLevel) {
	realp1=A+d;
	realp2=realp1+LastPlane_*(d+1);
	j=0;
	while (realp1<=realp2) {
	    if (fabs(*realp1)<EPSILON_LASS) j++;
	    realp1+=d+1;
	}
	if (d-j>=LaShift) shift_P(A, LastPlane_, d);
    }


    redA = (rational *) my_malloc (LastPlane_* d*sizeof(rational));
    if (redA == NULL) {
	fprintf (stderr, "\n***** ERROR/WARNING: Out of memory in 'lass.*redA'\n");
	exit(0);
    }
#ifdef ReverseLass
    for (row=LastPlane_; row>=0; row--) {
#else
    for (row=0; row<=LastPlane_; row++) {
#endif
	if (fabs(*(A+row*(d+1)+d))<EPSILON_LASS) 
            continue;                        /* skip this constraint if b_row == 0 */
	if (Index_needed)
	{  baserow=add_reduced_index(row, NULL, All_index);
           p2c[G_d-d][1] = baserow;
	   add_hypervar (baserow, G_d+1, &key);
	}	
	memcpy(&pivotrow[0], A+row*(d+1), sizeof(rational)*(d+1));
	col=0;                               /* search for pivot column */
	for (i=0; i<d; i++) {        
#if PIVOTING_LASS == 0
	    if (fabs(pivotrow[i])>=MIN_PIVOT_LASS) {col=i; break;};
#endif
	    if (fabs(pivotrow[i])>fabs(pivotrow[col])) col=i;
	};
	if (G_Storage>(G_d-d-1))
	{  basecol=add_reduced_index(col, NULL, Pivot);
           p2c[G_d-d][0] = basecol;
	   add_hypervar (G_m+1, basecol, &key);
	}

        /* copy A onto redA and at the same time perform pivoting */
	 
	mi=1.0/pivotrow[col];
	for (i=0; i<=d; i++) pivotrow[i]*=mi;
	realp1=A;
	realp2=redA;
	for (i=0; i<=LastPlane_; i++) {
	    if (i==row) {
		realp1+=d+1;
		continue;
	    };
	    mi=*(A+(i*(d+1))+col);
	    for (j=0; j<=d; j++) {
		if (j==col) {
		    realp1++;
		    continue;
		};
		*realp2=(*realp1)-pivotrow[j]*mi;
		realp1++;
		realp2++;
	    };
	};
	ma+= *(A+row*(d+1)+d)/(d*fabs(*(A+row*(d+1)+col)))
	     *lass(redA, LastPlane_-1, d-1);
        if (Index_needed)
        {  rm_original_inElAll_index(baserow);
           delete_hypervar (baserow, G_d+1, &key);
        }
	if (G_Storage>(G_d-d-1))
	{  del_original(basecol, Pivot);
	   delete_hypervar (G_m+1, basecol, &key);
	}
        #ifdef verboseFirstLevel
            if (d==G_d) 
	        printf("\nVolume accumulated to iteration %i is %20.12f",row,ma );
        #endif
    };
    my_free (redA, LastPlane_* d * sizeof (rational));
    label2: 
    if (Index_needed) {
	del_original_indices(Del_index, All_index);
        my_free (Del_index, (LastPlane_ + 2) * sizeof (T_LassInt));
    };
    if (store_volume)(*volume)=ma;
    return ma;
}

/****************************************************************************************/

void volume_lasserre_file (rational *volume, const char *input, int length)

{  int i;

   read_hyperplanes (input, length);

   if (G_m > 254)
   {  fprintf (stderr, "\n***** ERROR: Trying to use 'rlass' with more than 254 hyperplanes.");
      fprintf (stderr, "\nThis restriction can be changed, though. Please contact the authors.\n");
      exit (0);
   }
   if (G_Storage > G_d - 3)
      G_Storage = G_d - 3;
      /* necessary to prevent memory waste because in the tree arrays of length         */
      /* G_Storage + 2 are allocated                                                    */

   pivotrow = (rational *) my_malloc ((G_d + 1) * sizeof (rational));
   All_index = (T_LassInt *) my_malloc ((G_m + 1) * sizeof (T_LassInt));
   Pivot = (T_LassInt *) my_malloc ((G_d + 1) * sizeof (T_LassInt));
   p2c = (int **) my_malloc (G_d * sizeof (int *));
   for (i=0; i<G_d; i++){
       p2c[i] = (int *) my_malloc (2 * sizeof (int));
   }
   A=compact();
   planescopy=compact();
   tree_volumes = NULL;
   create_key (&key, KEY_PLANES_VAR);
   key.hypervar.hyperplanes [0] = G_m + 1;
   key.hypervar.variables [0] = G_d + 1;
   All_index[0]=G_m+2;  /* initialization (end mark) */
   Pivot[0]=G_m+2;	/* initialization (end mark) */

   *volume = lass (A, G_m-1, G_d);

/*
   free_key (key, KEY_PLANES_VAR);
*/
}

/****************************************************************************************/
