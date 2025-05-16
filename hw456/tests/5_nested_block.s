{
  int x;
  int y;
  
  x = 10;
  y = 20;
  
  print (x + y);
  
  /* 첫 번째 중첩 블록 */
  {
    int z;
    z = x * y;
    print (z);
    
    /* 두 번째 중첩 블록 */
    {
      int w;
      w = z - x;
      print (w);
      
      /* 세 번째 중첩 블록 */
      {
        x = w + y;
        print (x);
      }
    }
    
    /* 바깥 변수 값 변경 확인 */
    print (x);
  }
  
  /* 최상위 블록으로 돌아옴 */
  print (x + y);
}