/* Test: HARD_TRUE_HQ1 */
/* Desc: Loop with conditional refinement, safe array access, and safe division. */
/*       Tests loop analysis, pruning, and multiple safe operations. */
/* Expected: true */
{
  int[20] arr;
  int i;
  int x;
  int y;
  int z;

  i = 0;
  x = 15; /* x is [15,15] */
  y = 10; /* y is [10,10] */

  while (i < 10) { /* i is [0,9] in loop, [10,10] after */
    if (x > y) { /* True: x is [15,15], y is [10,10]. Pruning on x, y (no change here) */
      arr[i] = x - y; /* arr[i] = [5,5]. Access arr[[0,9]] is safe. */
      z = x / (i + 1); /* Divisor (i+1) is [1,10]. Safe. z is [1,15]. */
    } else {
      arr[i] = 0; /* Dead branch for these initial values */
      z = 0;
    }
    i = i + 1;
    x = x - 1; /* x becomes [5,15] over loop iterations, y fixed [10,10] */
               /* x > y condition will eventually become false. */
               /* When x=10, y=10, (x>y) is false. arr[i]=0. i can be up to 9. Safe. */
  }
  print(z); /* z's interval after loop is a join of [1,15] and [0,0] */
}