/* Test: IMP_TRUE_S4 (Corrected for S syntax, particularly '!=' operator) */
/* Desc: The condition !(an_even_x == an_odd_y) is ALWAYS TRUE. */
/*       The 'else' branch is an empty block (safe skip). Program is safe. */
/*       Interval analysis sees condition as !(Top == Top) -> !(Top) -> Top. */
/*       It cannot prove the condition is always true, thus cannot guarantee safety. */
/* Expected: false */
{
  int an_even_x;
  int an_odd_y;
  int input_val;
  int[5] arr_s4;
  int temp_div_2; 

  read(input_val); 
  
  temp_div_2 = input_val / 2;    
  an_even_x = temp_div_2 * 2;    
  an_odd_y  = an_even_x + 1;     

  /* S language equivalent of an_even_x != an_odd_y is !(an_even_x == an_odd_y) */
  if (!(an_even_x == an_odd_y)) 
  { /* This is the 'then' stmt: a BLOCK */
    arr_s4[0] = 1; /* Safe access. */
  }
  else
  { /* This is the 'else' stmt: an empty BLOCK, representing a skip */
    /* No operations here. This is BLOCK([], []) */
  }
  print(arr_s4[0]); 
}