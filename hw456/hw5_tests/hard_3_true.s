/* Test: HARD_TRUE_HQ3 */
/* Desc: Read input, prune effectively, perform safe arithmetic and array access. */
/* Expected: true */
{
  int[50] data;
  int x;
  int y;
  int idx;

  read(x); /* x is [-inf, +inf] */

  if (x > 0 && x <= 10) { /* Pruning: x becomes [1,10] */
    y = x * 3;         /* y becomes [3,30] */
    idx = y + 5;       /* idx becomes [8,35] */
    data[idx] = x;     /* Access data[[8,35]] is safe for data[50]. */
    print(idx);
  } else {
    /* In this path, x is [-inf,0] U [11,+inf]. No array access. */
    print(0);
  }
}