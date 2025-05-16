{
  /* 변수 선언 */
  int n;             /* 배열 크기 */
  int i;             /* 반복문 인덱스 */
  int pivot;         /* 피벗 값 */
  int temp;          /* 교환을 위한 임시 변수 */
  int low;           /* 현재 분할의 시작 인덱스 */
  int high;          /* 현재 분할의 끝 인덱스 */
  int[100] arr;      /* 정렬할 배열 */
  int input;         /* 입력값을 위한 임시 변수 */
  int[100] stack;    /* 분할 인덱스를 저장할 스택 */
  int top;           /* 스택의 최상위 위치 */
  int left;          /* 파티션 함수의 왼쪽 인덱스 */
  int right;         /* 파티션 함수의 오른쪽 인덱스 */
  int p;             /* 파티션 함수의 피벗 인덱스 */
  
  /* 배열 크기 입력 받기 */
  read (n);
  
  /* 배열 요소 입력 받기 */
  i = 0;
  while (i < n) {
    read (input);
    arr[i] = input;
    i = i + 1;
  }
  
  /* 퀵 정렬 구현 (비재귀 방식) */
  top = -1;        /* 스택 초기화 */
  
  /* 초기 범위를 스택에 푸시 */
  top = top + 1;
  stack[top] = 0;   /* low */
  top = top + 1;
  stack[top] = n - 1; /* high */
  
  /* 스택이 비어 있지 않은 동안 반복 */
  while (top >= 0) {
    /* 스택에서 high, low를 팝 */
    high = stack[top];
    top = top - 1;
    low = stack[top];
    top = top - 1;
    
    /* 파티션 수행 */
    /* 피벗으로 배열의 마지막 요소 선택 */
    pivot = arr[high];
    i = low - 1;
    
    /* 피벗보다 작은 요소들을 앞쪽으로 이동 */
    left = low;
    while (left < high) {
      if (arr[left] <= pivot) {
        i = i + 1;
        
        /* arr[i]와 arr[left] 교환 */
        temp = arr[i];
        arr[i] = arr[left];
        arr[left] = temp;
      }
      left = left + 1;
    }
    
    /* 피벗을 올바른 위치로 이동 */
    i = i + 1;
    temp = arr[i];
    arr[i] = arr[high];
    arr[high] = temp;
    
    /* 파티션 인덱스 저장 */
    p = i;
    
    /* 왼쪽 부분 배열이 존재하면 스택에 푸시 */
    if (p - 1 > low) {
      top = top + 1;
      stack[top] = low;
      top = top + 1;
      stack[top] = p - 1;
    }
    
    /* 오른쪽 부분 배열이 존재하면 스택에 푸시 */
    if (p + 1 < high) {
      top = top + 1;
      stack[top] = p + 1;
      top = top + 1;
      stack[top] = high;
    }
  }
  
  /* 정렬된 배열 출력 */
  i = 0;
  while (i < n) {
    print (arr[i]);
    i = i + 1;
  }
}