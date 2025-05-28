/* Test: IMP_FALSE_MV1 (Multi-Variable Assume - Difficulty 1) */
/* Desc: Simple equality. Program is ACTUALLY SAFE if condition holds (idx=5). */
/*       Intervals for x, y remain [-inf,+inf]. x-y is [-inf,+inf]. */
/* Expected: false */
{
  int[10] arr;
  int x;
  int y;
  int idx;

  read(x); /* x is [-inf,+inf] */
  read(y); /* y is [-inf,+inf] */

  if (x == y) { /* Assume x == y. Pruning doesn't constrain x or y to constants. */
    idx = x - y + 5; /* Actual: 0 + 5 = 5. Intervals: Top - Top + [5,5] = Top. */
    arr[idx] = 0;  /* arr[Top] is unsafe. */
  }
}