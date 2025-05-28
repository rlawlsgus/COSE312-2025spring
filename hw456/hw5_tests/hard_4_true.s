/* Test: HARD_TRUE_HQ4 */
/* Desc: Safe usage of uninitialized variable after it's correctly constrained by read and if. */
/*       Analyzer must handle uninitialized vars (Top) becoming constrained. */
/* Expected: true */
{
  int[10] safe_arr;
  int k; /* Uninitialized, so k is Top: [-inf, +inf] */
  int check_val;

  read(k); /* k is still Top: [-inf, +inf] */
  check_val = 5;

  if (k >= 0 && k < 10) { /* Pruning: k becomes [0,9] */
    if (k == check_val) { /* Pruning: k becomes [5,5] */
      safe_arr[k] = 100;  /* Access safe_arr[5], safe. */
      print(k);
    }
  }
}