{
  /* 변수 선언 */
  int n;           /* 배열 크기 */
  int i;           /* 외부 루프 인덱스 */
  int j;           /* 내부 루프 인덱스 */
  int temp;        /* 교환을 위한 임시 변수 */
  int swapped;     /* 최적화를 위한 스왑 여부 확인 변수 */
  int[15] arr;     /* 정렬할 배열 */
  int input;       /* 입력값을 위한 임시 변수 */
  
  /* 배열 크기 입력 받기 (최대 15) */
  read (n);
  
  /* 배열 요소 입력 받기 */
  i = 0;
  while (i < n) {
    read (input);  /* 입력값을 임시 변수에 저장 */
    arr[i] = input;  /* 배열에 입력값 할당 */
    i = i + 1;
  }
  
  /* 버블 정렬 구현 */
  i = 0;
  while (i < n - 1) {
    swapped = 0;
    j = 0;
    
    /* do-while 루프를 사용한 내부 반복 */
    do {
      /* 인접한 요소 비교 및 교환 */
      if (arr[j] > arr[j + 1]) {
        /* 값 교환 */
        temp = arr[j];
        arr[j] = arr[j + 1];
        arr[j + 1] = temp;
        swapped = 1;
      }
      else {
        /* 아무것도 하지 않음 */
        temp = temp;
      }
      
      j = j + 1;
    } while (j < n - i - 1);
    
    /* 최적화: 교환이 없으면 이미 정렬된 상태 */
    if (!swapped) {
      i = n - 1;  /* 루프 탈출을 위한 값 설정 */
    }
    else {
      i = i + 1;
    }
  }
  
  /* 정렬된 배열 출력 */
  print (-1);  /* 구분자(-1) 출력 */
  
  i = 0;
  while (i < n) {
    print (arr[i]);
    i = i + 1;
  }
}