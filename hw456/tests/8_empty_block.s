{
  int x;
  x = 10;
  
  /* 빈 블록 */
  {
  }
  
  print (x);
  
  /* 중첩된 빈 블록 */
  {
    {
    }
    {
      {
      }
    }
  }
  
  print (x + 5);
}