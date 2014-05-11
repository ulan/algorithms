#include <cstdio>
#include <cstdlib>
#include <vector>
using namespace std;

const int N = 1000;

int adj[N][N];

int main() {
  int n = 1000;
  int m = 100000;
  for (int i = 0; i < m; i++) {
    int a = rand() % n;
    int b = rand() % n;
    int w = rand() % 20 + 1;
    if (a != b) {
       adj[a][b] = w;
    }
  }
  m = 0;
  for (int i = 0; i < n; i++)
   for (int j = 0; j < n; j++)
     if (adj[i][j]) m++;

  printf("%d %d\n", n, m);
  for (int j = 0; j < n; j++)
   for (int i = 0; i < n; i++)
     if (adj[i][j]) printf("%d %d %d\n", i + 1, j + 1, adj[i][j]); 
  return 0;
}
