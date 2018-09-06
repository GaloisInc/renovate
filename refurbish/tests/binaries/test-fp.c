#include "util.h"
#include <stdio.h>


void _start() {
  // gcc will init via memset(): float res [] = {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
  float res[8];
  float a=45.4; // r9
  float b=1.1;  // r10
  float c=0.0;
  float *pointer = &res[0]; // r10
  // gcc will init via memset(): double dres [] = {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
  double dres[8];
  double da = 41.221;
  double db = 11.11111;
  double dc = 0.0;
  double *dpointer = &dres[0];
  int idx1 = 4;
  int idx2 = 0;
  int idx3 = 8;

  res[0] = 0.0;
  res[1] = 0.0;
  res[2] = 0.0;
  res[3] = 0.0;
  res[4] = 0.0;
  res[5] = 0.0;
  res[6] = 0.0;
  res[7] = 0.0;

  dres[0] = 0.0;
  dres[1] = 0.0;
  dres[2] = 0.0;
  dres[3] = 0.0;
  dres[4] = 0.0;
  dres[5] = 0.0;
  dres[6] = 0.0;
  dres[7] = 0.0;

#ifdef __powerpc__
  /* TEST: Load/store single precision D-form */
  asm ( " stfs    %0,0(%2)        \n"
	" stfs    %1,4(%2)        \n"
	" lfs     %1,0(%2)        \n"
	" lfs     %0,4(%2)        \n"
	" stfsu   %0,0(%2)        \n"
	" stfsu   %1,4(%2)        \n"
	" lfsu    %1,-4(%2)       \n"
	" lfsu    %0,4(%2)        \n"
	: "+r"(a), "+r"(b)
	: "r"(pointer));

  /* TEST: Load/store single precision X-form */
  asm ( " stfsx   %0,%2,%3        \n"
	" stfsx   %1,%2,%4        \n"
	" lfsx    %1,%2,%3        \n"
	" lfsx    %0,%2,%4        \n"
	" stfsux  %0,%2,%3        \n"
	" stfsux  %1,%2,%4        \n"
	" lfsux   %1,%2,%3        \n"
	" lfsux   %0,%2,%4        \n"
	: "+r"(a), "+r"(b)
	: "r"(pointer), "r"(idx1), "r"(idx2));

  /* TEST: Load/store double precision D-form */
  asm ( " stfd    %0,0(%2)        \n"
	" stfd    %1,4(%2)        \n"
	" lfd     %1,0(%2)        \n"
	" lfd     %0,4(%2)        \n"
	" stfdu   %0,0(%2)        \n"
	" stfdu   %1,4(%2)        \n"
	" lfdu    %1,-4(%2)       \n"
	" lfdu    %0,4(%2)        \n"
	: "+r"(da), "+r"(db)
	: "r"(dpointer));

  /* TEST: Load/store single precision X-form */
  asm ( " stfdx   %0,%2,%3        \n"
	" stfdx   %1,%2,%4        \n"
	" lfdx    %1,%2,%3        \n"
	" lfdx    %0,%2,%4        \n"
	" stfdux  %0,%2,%3        \n"
	" stfdux  %1,%2,%4        \n"
	" lfdux   %1,%2,%3        \n"
	" lfdux   %0,%2,%4        \n"
	: "+r"(da), "+r"(db)
   : "r"(dpointer), "r"(idx3), "r"(idx2));
#endif

  EXIT(0);
}
