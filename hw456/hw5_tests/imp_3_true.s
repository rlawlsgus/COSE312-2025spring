/* Test: IMP_TRUE_S3 (Actually Safe, Analyzer Expected: false) */
/* Desc: Indexing based on a square modulo N. (val*val) % 3 is always 0 or 1. */
/*       Array of size 2 is safe. */
/*       Interval analysis for (val*val) is [0,+inf]. The simulated modulo op on this yields Top. */
/* Expected: false */
{
  int[2] arr; /* Indices 0, 1 */
  int val;
  int val_sq;
  int div_val_sq_by_3;
  int mul_val;
  int idx;

  read(val);   /* val is [-inf,+inf] */
  val_sq = val * val; /* val_sq is [0,+inf] (if val is not Bot) */

  /* idx = val_sq % 3. Simulate: val_sq - (val_sq / 3) * 3 */
  div_val_sq_by_3 = val_sq / 3;     /* Intervals: [0,+inf] / [3,3] = [0,+inf] */
  mul_val = div_val_sq_by_3 * 3;    /* Intervals: [0,+inf] * [3,3] = [0,+inf] */
  idx = val_sq - mul_val;           /* Intervals: [0,+inf] - [0,+inf] = [-inf,+inf] = Top */

  arr[idx] = 123; /* Actual idx is 0 or 1. Safe. Analyzer sees arr[Top]. */
}