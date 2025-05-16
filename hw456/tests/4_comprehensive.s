{
  /* 다양한 변수 선언 */
  int n;
  int i;
  int j;
  int temp;
  int result;
  int[20] arr;
  int[20] fib;
  int sum;
  int flag;
  int is_even;
  
  /* 입력 받기 */
  read (n);
  
  /* 기본 배열 초기화 */
  i = 0;
  while (i < n && i < 20) {
    arr[i] = i * i;
    i++;
  }
  
  /* 피보나치 수열 계산 (첫 번째 테스트) */
  fib[0] = 0;
  fib[1] = 1;
  i = 2;
  while (i < n && i < 20) {
    fib[i] = fib[i-1] + fib[i-2];
    i++;
  }
  
  /* 복잡한 산술 표현식 (두 번째 테스트) */
  result = (n * 3 + 2) * (n - 1) / 2;
  print (result);
  
  /* 다중 중첩 제어 구조 (세 번째 테스트) */
  sum = 0;
  i = 0;
  flag = 0;
  
  if (n > 5) {
    while (i < n) {
      j = 0;
      while (j < i) {
        /* 짝수/홀수 판별을 위한 로직 (% 연산자 대신) */
        temp = (i + j) / 2;
        is_even = (i + j) == temp * 2;
        
        if (is_even) {
          sum = sum + (i * j);
        }
        else {
          if (flag == 0) {
            sum = sum + i + j;
            flag = 1;
          }
          else {
            sum = sum - i - j;
            flag = 0;
          }
        }
        j++;
      }
      i++;
    }
  }
  else {
    do {
      sum = sum + i * i;
      i++;
    } while (i < n);
  }
  
  print (sum);
  
  /* 복잡한 배열 인덱싱 (네 번째 테스트) */
  i = 1;
  j = 2;
  if (i + j * 2 < n && i + j * 2 < 20) {
    arr[i + j * 2] = sum;
    print (arr[i + j * 2]);
  }
  else {
    print (-1);
  }
  
  /* 짝수 판별과 복잡한 조건 검사 (다섯 번째 테스트) */
  i = 0;
  while (i < n && i < 20) {
    /* 짝수 판별 로직 (% 연산자 대신) */
    temp = fib[i] / 2;
    is_even = fib[i] == temp * 2;
    
    if (is_even) {
      /* 짝수 피보나치 */
      print (fib[i]);
    }
    else {
      if (arr[i] > fib[i]) {
        print (arr[i] - fib[i]);
      }
      else {
        print (fib[i] - arr[i]);
      }
    }
    i++;
  }
  
  /* 최종 결과 출력 */
  print (9999);  /* 종료 표시 */
}