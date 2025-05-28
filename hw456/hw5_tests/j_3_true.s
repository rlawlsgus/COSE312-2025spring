{
  int[10] data;
  int x;
  
  read(x);
  if (x >= 0) {
    if (x < 10) {
      data[x] = 100;
    }
  }
  
  print(data[0]);
}