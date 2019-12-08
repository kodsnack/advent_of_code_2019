#include <stdio.h>
#include <queue>
#include <stdio.h>
#include <string.h>
int hash(const char *str){
	int res=0;
	const char *v=str;
	while(*v){
		res=res*36+((*v<='9'&&*v>='0')?*v-'0':*v-'A'+10);
		v++;
	}
	return res;
}
struct node{
	int t,w,x;
}g[300000];
struct qnode{
	int i,w;
	qnode(int i,int w){
		this->i=i;
		this->w=w;
	}
	bool operator<(const qnode& p)const{
		return w>p.w;
	}
};
int head[50000],dis[50000],vis[50000],c;
void dijkstra(int s){
	std::priority_queue<qnode> q;
	memset(dis,0x3f,sizeof(dis));
	dis[s]=0;
	q.push(qnode(s,0));
	while(!q.empty()){
		qnode p=q.top();
		q.pop();
		int nnode=p.i;
		if(vis[nnode])continue;
		vis[nnode]=true;
		for(int i=head[nnode];i;i=g[i].x){
			const node &w=g[i];
			if(w.w+dis[nnode]<dis[w.t]){
				q.push(qnode(w.t,dis[w.t]));
				dis[w.t]=w.w+dis[nnode];
			}
		}
	}
}
inline void add_edge(int f,int t,int w){
	c++;
	g[c].t=t;
	g[c].w=w;
	g[c].x=head[f];
	head[f]=c;
}
char ch1[10],ch2[10];
long long ans=0;
int main(){
	freopen("Input.txt","r",stdin);
	while(scanf(" %[^)])%[^\n]",ch1,ch2)==2){
		int hash1=hash(ch1);
		int hash2=hash(ch2);
		add_edge(hash1,hash2,1);
		add_edge(hash2,hash1,1);
	}
	int hash_you=hash("YOU");
	int hash_san=hash("SAN");
	dijkstra(hash_you);
	printf("%d",dis[hash_san]-2);
	return 0;
}
