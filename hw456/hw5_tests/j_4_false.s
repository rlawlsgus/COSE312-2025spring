{
  int[3] small;
  int i;
  
  i = 0;
  while (i <= 3) {  /* Error: i=3 when accessing small[3] */
    small[i] = i;
    i = i + 1;
  }
}