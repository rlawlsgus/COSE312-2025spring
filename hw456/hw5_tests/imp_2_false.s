/* Test: IMP_FALSE_MV2 (Multi-Variable Assume - Difficulty 2) */
/* Desc: Linear relation. Program is ACTUALLY SAFE if condition holds (idx=1). */
/*       y = 2x.  2x - y + 1 = 2x - 2x + 1 = 1. */
/*       Intervals: 2*Top - Top + [1,1] = Top. */
/* Expected: false */
{
  int[10] arr;
  int x;
  int y;
  int idx;
  int two_x;

  read(x); /* x is [-inf,+inf] */
  read(y); /* y is [-inf,+inf] */
  
  two_x = 2 * x; /* two_x is [-inf,+inf] */

  if (y == two_x) { /* Assume y == 2x. Pruning unlikely to deduce tight bounds for x or y. */
    idx = two_x - y + 1; /* Actual: 1. Intervals: Top - Top + [1,1] = Top. */
    arr[idx] = 0; /* arr[Top] is unsafe. */
  }
}
