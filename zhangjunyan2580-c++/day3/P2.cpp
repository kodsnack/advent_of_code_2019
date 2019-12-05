#include <stdio.h>
#include <ctype.h>
#include <map>
struct point{
	int x,y;
	bool operator<(const point& a)const{
		return x==a.x?y<a.y:x<a.x;
	} 
	bool operator==(const point& a)const{
		return x==a.x&&y==a.y;
	}
}pos,lpos;
std::map<point,int> lstate,tstate;
int dis,ans=1000000000,step;
char dir;
bool read(){
	static char ch=' ';
	while(ch==','||isspace(ch))ch=getchar();
	dir=ch;ch=getchar();dis=0;
	while(ch<'0'||ch>'9')ch=getchar();
	while(ch>='0'&&ch<='9')dis=dis*10+ch-'0',ch=getchar();
	return ch==',';
}
inline void Tmin(int &a,int b){if(a>b)a=b;}
int main(){
	freopen("Input.txt","r",stdin);
	pos.x=0;pos.y=0;tstate[pos]=0;
	lpos.x=0;lpos.y=0;
	while(read())
		for(int k=0;k<dis;k++){
			switch(dir){
				case 'L':pos.y--;break;
				case 'R':pos.y++;break;
				case 'U':pos.x--;break;
				case 'D':pos.x++;break;
			}
			lstate[pos]=lstate[lpos]+1;
			if(!tstate.count(pos))tstate[pos]=lstate[pos];
			lpos=pos;
		}
	for(int k=0;k<dis;k++){
		switch(dir){
			case 'L':pos.y--;break;
			case 'R':pos.y++;break;
			case 'U':pos.x--;break;
			case 'D':pos.x++;break;
		}
		lstate[pos]=lstate[lpos]+1;
		if(!tstate.count(pos))tstate[pos]=lstate[pos];
		lpos=pos;
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
			step++;
			if(tstate.count(pos))Tmin(ans,step+tstate[pos]);
		}
	for(int k=0;k<dis;k++){
		switch(dir){
			case 'L':pos.y--;break;
			case 'R':pos.y++;break;
			case 'U':pos.x--;break;
			case 'D':pos.x++;break;
		}
		step++;
		if(tstate.count(pos))Tmin(ans,step+tstate[pos]);
	}
	printf("%d",ans);
	return 0;
}
