{
  int[10] numbers;
  int counter;
  
  counter = 1;
  while (counter < 11) {  /* Error: counter=10 accesses numbers[10] */
    numbers[counter] = counter * counter;
    counter = counter + 1;
  }
}