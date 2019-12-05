#include <stdio.h>
#include <ctype.h>
#include <set>
struct point{
	int x,y;
	bool operator<(const point& a)const{
		return x==a.x?y<a.y:x<a.x;
	} 
	bool operator==(const point& a)const{
		return x==a.x&&y==a.y;
	}
}pos;
std::set<point> state;
int dis,ans=1000000000;
char dir;
bool read(){
	static char ch=' ';
	while(ch==','||isspace(ch))ch=getchar();
	dir=ch;ch=getchar();dis=0;
	while(ch<'0'||ch>'9')ch=getchar();
	while(ch>='0'&&ch<='9')dis=dis*10+ch-'0',ch=getchar();
	return ch==',';
}
inline int abs(int v){return v>0?v:-v;}
inline void Tmin(int &a,int b){if(a>b)a=b;}
int main(){
	freopen("Input.txt","r",stdin);
	pos.x=0;pos.y=0;
	while(read())
		for(int k=0;k<dis;k++){
			switch(dir){
				case 'L':pos.y--;break;
				case 'R':pos.y++;break;
				case 'U':pos.x--;break;
				case 'D':pos.x++;break;
			}
			state.insert(pos);
		}
	for(int k=0;k<dis;k++){
		switch(dir){
			case 'L':pos.y--;break;
			case 'R':pos.y++;break;
			case 'U':pos.x--;break;
			case 'D':pos.x++;break;
		}
		state.insert(pos);
	}
	pos.x=0;pos.y=0;
	while(read())
		for(int k=0;k<dis;k++){
			switch(dir){
				case 'L':pos.y--;break;
				case 'R':pos.y++;break;
				case 'U':pos.x--;break;
				case 'D':pos.x++;break;
			}
			if(state.count(pos))Tmin(ans,abs(pos.x)+abs(pos.y));
		}
	for(int k=0;k<dis;k++){
		switch(dir){
			case 'L':pos.y--;break;
			case 'R':pos.y++;break;
			case 'U':pos.x--;break;
			case 'D':pos.x++;break;
		}
		if(state.count(pos))Tmin(ans,abs(pos.x)+abs(pos.y));
	}
	printf("%d",ans);
	return 0;
} 
