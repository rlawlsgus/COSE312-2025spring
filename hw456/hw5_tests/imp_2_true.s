/* Test: IMP_TRUE_S2 (Actually Safe, Analyzer Expected: false) */
/* Desc: Modulo property for index. idx_mod = val % 1, which is always 0. */
/*       Array tiny_arr[1] is safe with index 0. */
/*       Interval arithmetic on val (Top) results in idx_mod being Top. */
/* Expected: false */
{
  int[1] tiny_arr;
  int val;
  int val_div_1;
  int val_mul_1;
  int idx_mod;

  read(val); /* val is [-inf, +inf] */
  
  /* Simulate val % 1: idx_mod = val - (val / 1) * 1 */
  val_div_1 = val / 1;          /* Intervals: Top / [1,1] = Top */
  val_mul_1 = val_div_1 * 1;    /* Intervals: Top * [1,1] = Top */
  idx_mod = val - val_mul_1;    /* Actual: 0. Intervals: Top - Top = Top. */
  
  tiny_arr[idx_mod] = 0; /* Safe. tiny_arr[Top] for analyzer. */
}
