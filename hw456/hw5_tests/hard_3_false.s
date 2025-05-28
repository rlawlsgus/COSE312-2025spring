/* Test: HARD_FALSE_HQ3 */
/* Desc: Clear type errors: int used as array, array in arithmetic, array as conditional. */
/* Expected: false */
{
  int[10] my_arr;
  int my_int;
  int another_int;

  my_int = 5;
  another_int = 0;

  my_int[0] = 1;      /* BUG: my_int is not an array. */
  another_int = my_arr + my_int; /* BUG: Array + Int. */
  print(another_int);

  if (my_arr) {       /* BUG: Array as conditional. */
    print(1);
  }
}
