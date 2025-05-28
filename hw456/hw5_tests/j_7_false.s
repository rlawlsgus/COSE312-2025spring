{
  int[8] table;
  int x;
  int y;
  
  read(x);
  read(y);
  
  if (x > 0) {
    if (y < 20) {
      table[x + y] = 1;  /* Error: x+y could be >= 8 */
    }
  }
}