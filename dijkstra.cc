#include <vector>
#include <cstdio>
#include <algorithm>

using namespace std;

const int infinity = 100000000;
const int N = 1000;
vector<pair<int,int> > adj[N];
int dist[N];
bool visited[N];

int main() {
  int n, m;
  scanf("%d %d", &n, &m);
  for (int i = 0; i < m; i++) {
    int a, b, c;
    scanf("%d %d %d", &a, &b, &c);
    adj[a - 1].push_back(make_pair(b - 1, c));
  }
  for (int i = 0; i < n; i++) dist[i] = infinity;
  for (int i = 0; i < n; i++) visited[i] = false;
  dist[0] = 0;
  while (true) {
    int v = -1;
    for (int i = 0; i < n; i++) {
      if (!visited[i] && (v == -1 || dist[i] < dist[v])) v = i;
    }
    if (v == -1) break;
    visited[v] = true;
    for (size_t i = 0; i < adj[v].size(); i++) {
       int nv = adj[v][i].first;
       int nw = adj[v][i].second;
       dist[nv] = min(dist[nv], dist[v] + nw);
    }
  }
  printf("%d\n", dist[n-1]);
  return 0;
}
