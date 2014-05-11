#include <vector>
#include <cstdio>
#include <algorithm>
#include <set>

using namespace std;

const int infinity = 100000000;
const int N = 1000;
typedef int Vertex;
typedef int Weight;
typedef int Distance;
typedef set<pair<Distance, Vertex> > PriorityQueue;

vector<pair<Vertex,Weight> > adj[N];

int n, m;

void read_graph() {
  scanf("%d %d", &n, &m);
  for (int i = 0; i < m; i++) {
    int a, b, c;
    scanf("%d %d %d", &a, &b, &c);
    adj[a - 1].push_back(make_pair(b - 1, c));
  }
}

pair<Distance, Vertex> pop_min(PriorityQueue* pq) {
  PriorityQueue::iterator it = pq->begin();
  pair<Distance, Vertex> result = *it;
  pq->erase(it);
  return result;
}

Distance dist[N];
void adjust(PriorityQueue* pq, Vertex k, Distance d) {
  pq->erase(make_pair(dist[k], k));
  pq->insert(make_pair(d, k));
  dist[k] = d;
}


int main() {
  read_graph();
  PriorityQueue pq;
  for (int i = 0; i < n; i++) {
    dist[i] = infinity;
    pq.insert(make_pair(infinity, i));
  }
  adjust(&pq, 0, 0);
  while (!pq.empty()) {
    pair<Distance, Vertex> dv = pop_min(&pq);
    Vertex v = dv.second;
    for (size_t i = 0; i < adj[v].size(); i++) {
       Vertex nv = adj[v][i].first;
       Weight nw = adj[v][i].second;
       if (dist[nv] > dist[v] + nw) {
          adjust(&pq, nv, dist[v] + nw);
       }
    }
  }
  printf("%d\n", dist[n-1]);
  return 0;
}
