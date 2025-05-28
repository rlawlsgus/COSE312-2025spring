/* Test: HARD_FALSE_HQ1 */
/* Desc: Loop causes out-of-bounds. Also contains a conditional division by zero. */
/*       Analyzer should detect at least one error. */
/* Expected: false */
{
  int[10] arr;
  int i;
  int divisor;

  i = 0;
  divisor = 5; /* Initially safe */

  while (i < 12) { /* i goes from [0,0] to [11,11] */
    arr[i] = i;    /* BUG: arr[10] and arr[11] are OOB. */
    if (i == 10) {
      divisor = 0; /* divisor becomes [0,0] */
    }
    print(100 / divisor); /* BUG: Div by zero when i=10 (or 11). */
                          /* If OOB is found first, that's fine. */
    i = i + 1;
  }
}