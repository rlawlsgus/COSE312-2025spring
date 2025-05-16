{
  /* 변수 선언 */
  int n;            /* 정점의 수 */
  int m;            /* 간선의 수 */
  int i;            /* 반복문 인덱스 */
  int j;            /* 반복문 인덱스 */
  int u;            /* 시작 정점 */
  int v;            /* 도착 정점 */
  int w;            /* 간선의 가중치 */
  int src;          /* 시작 정점 */
  int[400] graph;   /* 그래프 (1차원 배열로 2차원 행렬 표현, 최대 20x20) */
  int[20] dist;     /* 최단 거리 배열 */
  int[20] visited;  /* 방문 여부 배열 */
  int min_dist;     /* 최소 거리 */
  int min_vertex;   /* 최소 거리 정점 */
  int INF;          /* 무한대 값 */
  int temp;         /* 임시 변수 */
  int graph_value;  /* 그래프 값 임시 저장 변수 */
  
  /* 무한대 값 설정 (큰 값 사용) */
  INF = 9999;
  
  /* 정점 수, 간선 수, 시작 정점 입력 */
  read (n);
  read (m);
  read (src);
  
  /* 그래프 초기화 (모든 간선을 INF로 설정) */
  i = 0;
  while (i < n) {
    j = 0;
    while (j < n) {
      if (i == j) {
        graph[i * n + j] = 0;  /* 자기 자신으로의 거리는 0 */
      }
      else {
        graph[i * n + j] = INF;  /* 나머지는 무한대 */
      }
      j = j + 1;
    }
    i = i + 1;
  }
  
  /* 간선 정보 입력 (u->v with weight w) */
  i = 0;
  while (i < m) {
    read (u);
    read (v);
    read (w);
    graph[u * n + v] = w;  /* 가중치 그래프에 저장 */
    i = i + 1;
  }
  
  /* 최단 거리 배열과 방문 배열 초기화 */
  i = 0;
  while (i < n) {
    dist[i] = INF;
    visited[i] = 0;
    i = i + 1;
  }
  
  /* 시작 정점의 거리는 0 */
  dist[src] = 0;
  
  /* 다익스트라 알고리즘 구현 */
  i = 0;
  while (i < n - 1) {
    /* 방문하지 않은 정점 중 최소 거리를 가진 정점 찾기 */
    min_dist = INF;
    min_vertex = 0;
    j = 0;
    
    while (j < n) {
      if (visited[j] == 0) {
        if (dist[j] < min_dist) {
          min_dist = dist[j];
          min_vertex = j;
        }
      }
      j = j + 1;
    }
    
    /* 찾은 최소 거리 정점을 방문 표시 */
    visited[min_vertex] = 1;
    
    /* 인접한 정점들의 거리 갱신 */
    j = 0;
    while (j < n) {
      graph_value = graph[min_vertex * n + j];  /* 그래프 값을 임시 변수에 저장 */
      
      /* 방문하지 않았고, 간선이 존재하는 경우 */
      if (visited[j] == 0) {
        if (graph_value < INF) {  /* 간선이 존재하는 경우 (INF가 아님) */
          temp = dist[min_vertex] + graph_value;
          
          /* 더 짧은 경로를 발견한 경우 거리 갱신 */
          if (temp < dist[j]) {
            dist[j] = temp;
          }
        }
      }
      j = j + 1;
    }
    
    i = i + 1;
  }
  
  /* 모든 정점까지의 최단 거리 출력 */
  print (0);  /* 결과 출력 시작 표시 */
  
  i = 0;
  while (i < n) {
    if (dist[i] == INF) {
      /* 도달할 수 없는 정점 */
      print (-1);
    }
    else {
      /* 해당 정점까지의 최단 거리 */
      print (dist[i]);
    }
    i = i + 1;
  }
}