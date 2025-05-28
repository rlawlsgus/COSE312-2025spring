/* Test: HARD_TRUE_HQ5 (Additional High-Quality Case) */
/* Desc: Loop that initializes an array. Then, a conditional loop processes parts of it. */
/*       All accesses are safe. Involves multiple variables and conditions. */
/* Expected: true */
{
  int[30] data_array; /* Array of size 30, indices 0-29 */
  int i;
  int limit;
  int start_idx;
  int current_val;

  /* Initialize the entire array with increasing values */
  i = 0;
  while (i < 30) { /* Loop 1: i from [0,0] to [29,29] */
    data_array[i] = i * 10; /* Safe: data_array[[0,29]] = some_int */
    i = i + 1;
  }
  /* After Loop 1: i is [30,30] */

  limit = 20;      /* limit is [20,20] */
  start_idx = 5;   /* start_idx is [5,5] */
  current_val = 0;

  i = start_idx; /* i is reset to [5,5] */

  /* Loop conditionally processes a segment of the array */
  /* Loop runs if i ([5,5] initially) < limit ([20,20]) */
  while (i < limit && i >= 0) { /* Loop 2: i from [5,5] up to [19,19] */
                                /* Condition i >= 0 is redundant given i starts at 5 and increments, */
                                /* but good for testing pruning. */
                                /* Interval for i in loop: [5,19] */
    
    current_val = data_array[i]; /* Safe: Access data_array[[5,19]] */
    
    if (current_val > 100) { /* data_array[i] = i*10. So i*10 > 100 => i > 10. */
                             /* If i > 10 (i.e., i is [11,19]), then this branch is taken. */
      data_array[i] = current_val - 5; /* Safe modification */
    } else {
      /* If i <= 10 (i.e., i is [5,10]), this branch is taken. */
      data_array[i] = current_val + 5; /* Safe modification */
    }
    i = i + 1;
  }
  /* After Loop 2: if loop ran, i becomes [20,20]. If not (e.g., if start_idx >= limit), i remains [5,5]. */
  /* Join would make i [5,20] or similar, depending on initial values if they were symbolic. */
  /* Here, loop always runs, so i is [20,20]. */

  print(data_array[15]); /* Safe: Access data_array[15] */
  print(i); /* i will be [20,20] */
}