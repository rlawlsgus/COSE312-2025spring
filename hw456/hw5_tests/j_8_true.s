{
  int[5] arr;
  int x;
  
  read(x);
  if (x > 100) {
    if (x < 50) {  /* Dead code but still unsafe */
      arr[100] = 1;  /* Error: 100 >= 5 */
    }
  }
}