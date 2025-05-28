{
  int[20] buffer;
  int i;
  int j;
  int safe_index;
  
  i = 3;
  j = 2;
  safe_index = i + j;  /* 5 */
  
  if (safe_index < 20) {
    buffer[safe_index] = 999;
  }
  
  print(buffer[5]);
}