/* Test: IMP_FALSE_MV3 (Multi-Variable Assume - Difficulty 3) */
/* Desc: Non-linear relation (product). Program ACTUALLY SAFE if condition holds (idx=0). */
/*       y = x*z. x*z - y = 0. */
/*       Interval analysis for x*z is [0,+inf] if x,z can be anything, or wider. */
/*       x*z - y = Top - Top = Top. */
/* Expected: false */
{
  int[10] arr;
  int x;
  int y;
  int z;
  int idx;
  int prod_xz;

  read(x); /* x is [-inf,+inf] */
  read(y); /* y is [-inf,+inf] */
  read(z); /* z is [-inf,+inf] */

  prod_xz = x * z; /* prod_xz is [-inf,+inf] for interval arithmetic (considers signs) */
                   /* More precise: if x,z could be 0, then [0,0] is possible. */
                   /* But generally can be very wide. */

  if (y == prod_xz) { /* Assume y == x*z. */
    idx = prod_xz - y; /* Actual: 0. Intervals: Top - Top = Top. */
    arr[idx] = 0; /* arr[Top] is unsafe. */
  }
}