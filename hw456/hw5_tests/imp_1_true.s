/* Test: IMP_TRUE_S1 (Actually Safe, Analyzer Expected: false) */
/* Desc: Relational equality. Index x-y is always 0. */
/*       Interval analysis sees x,y as independent [-inf,+inf] after reads. */
/*       x-y becomes Top. arr[Top] -> reported as potential error. */
/* Expected: false */
{
  int[5] arr;
  int x;
  int y;
  int temp_idx;

  read(x); /* x is [-inf,+inf] */
  y = x;   /* y is also [-inf,+inf], but y == x is true */

  temp_idx = x-y; /* Actual index: 0. Intervals: Top - Top = Top. */
  arr[temp_idx] = 0; /* Safe. arr[Top] for analyzer. */
}