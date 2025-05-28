/* Test: HARD_FALSE_HQ2 */
/* Desc: Multiple potential errors: OOB from read, division by zero from read. */
/* Expected: false */
{
  int[5] data_arr;
  int x;
  int y;
  int result;

  read(x); /* x is [-inf, +inf] */
  read(y); /* y is [-inf, +inf] */

  data_arr[x] = 10; /* BUG: x can be OOB. */
  result = (x + y) / y; /* BUG: y can be 0. */
  print(result);
}
