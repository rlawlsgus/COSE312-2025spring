{
  int a;
  int b;
  int c;
  
  a = 5;
  b = 3;
  c = a - b;  /* c = 2 */
  
  while (c > 0) {
    c = c - 1;
  }
  /* Now c = 0 */
  
  print(b / c);  /* Error: division by zero */
}