#include <vector>
#include <stdio.h>
#include <string.h>
#include <algorithm>
int n,m,a,ans,x,y,rem,kth,lx,ly,fkth=200;
char dat[105][105];
struct point{
	int x,y;
};
std::vector<point> ru,rd,lu,ld,nru,nrd,nlu,nld;
bool fdu=true,fdr=true,fdl=true,fdd=true;
int gcd(int a,int b){return b?gcd(b,a%b):a;}
int main(){
	freopen("Input.txt","r",stdin);
	while(scanf(" %s",dat[n])==1)n++;
	m=strlen(dat[0]);
	for(int i=0;i<n;i++)
		for(int j=0;j<m;j++)
			if(dat[i][j]=='#'){
				a=0;rem++;
				for(int k=0;k<n;k++)
					for(int l=0;l<m;l++)
						if(gcd(k,l)==1){
							for(int ni=i+k,nj=j+l;ni<n&&nj<m;ni+=k,nj+=l)
								if(dat[ni][nj]=='#'){a++;break;}
							if(k)
								for(int ni=i-k,nj=j+l;ni>=0&&nj<m;ni-=k,nj+=l)
									if(dat[ni][nj]=='#'){a++;break;}
							if(l)
								for(int ni=i+k,nj=j-l;ni<n&&nj>=0;ni+=k,nj-=l)
									if(dat[ni][nj]=='#'){a++;break;}
							if(k&&l)
								for(int ni=i-k,nj=j-l;ni>=0&&nj>=0;ni-=k,nj-=l)
									if(dat[ni][nj]=='#'){a++;break;}
						}
				if(a>ans)ans=a,x=i,y=j;
			}
	for(int i=1;i<n;i++)
		for(int j=1;j<m;j++){
			ru.push_back((point){-i,j});
			rd.push_back((point){i,j});
			lu.push_back((point){-i,-j});
			ld.push_back((point){i,-j});
		}
	std::sort(ru.begin(),ru.end(),[](const point& a,const point& b)->bool{
		return (a.x*b.y<a.y*b.x)^(a.y<0)^(b.y<0);
	});
	std::sort(lu.begin(),lu.end(),[](const point& a,const point& b)->bool{
		return (a.x*b.y<a.y*b.x)^(a.y<0)^(b.y<0);
	});
	std::sort(rd.begin(),rd.end(),[](const point& a,const point& b)->bool{
		return (a.x*b.y<a.y*b.x)^(a.y<0)^(b.y<0);
	});
	std::sort(ld.begin(),ld.end(),[](const point& a,const point& b)->bool{
		return (a.x*b.y<a.y*b.x)^(a.y<0)^(b.y<0);
	});
	while(rem>1){
		int k=-1,l=0;
		bool find=false;
		if(fdu)
			for(int ni=x-1,nj=y;ni>=0;ni--)
				if(dat[ni][nj]=='#'){rem--;find=true;kth++;lx=ni;ly=nj;dat[ni][nj]='.';break;}
		fdu=find;
		if(kth==fkth){
			printf("%d",ly*100+lx);
			return 0;
		}
		for(const point& v:ru){
			find=false;
			if(gcd(-v.x,v.y)==1)
				for(int ni=x+v.x,nj=y+v.y;ni>=0&&nj<m;ni+=v.x,nj+=v.y)
					if(dat[ni][nj]=='#'){rem--;find=true;kth++;lx=ni;ly=nj;dat[ni][nj]='.';nru.push_back(v);break;}
			if(kth==fkth){
				printf("%d",ly*100+lx);
				return 0;
			}
		}
		find=false;
		if(fdr)
			for(int ni=x,nj=y+1;nj<m;nj++)
				if(dat[ni][nj]=='#'){rem--;find=true;kth++;lx=ni;ly=nj;dat[ni][nj]='.';break;}
		fdr=find;
		if(kth==fkth){
			printf("%d",ly*100+lx);
			return 0;
		}
		for(const point& v:rd){
			find=false;
			if(gcd(v.x,v.y)==1)
				for(int ni=x+v.x,nj=y+v.y;ni<n&&nj<m;ni+=v.x,nj+=v.y)
					if(dat[ni][nj]=='#'){rem--;find=true;kth++;lx=ni;ly=nj;dat[ni][nj]='.';nrd.push_back(v);break;}
			if(kth==fkth){
				printf("%d",ly*100+lx);
				return 0;
			}
		}
		find=false;
		if(fdd)
			for(int ni=x+1,nj=y;ni<n;ni++)
				if(dat[ni][nj]=='#'){rem--;find=true;kth++;lx=ni;ly=nj;dat[ni][nj]='.';break;}
		fdd=find;
		if(kth==fkth){
			printf("%d",ly*100+lx);
			return 0;
		}
		for(const point& v:ld){
			find=false;
			if(gcd(v.x,-v.y)==1)
				for(int ni=x+v.x,nj=y+v.y;ni<n&&nj>=0;ni+=v.x,nj+=v.y)
					if(dat[ni][nj]=='#'){rem--;find=true;kth++;lx=ni;ly=nj;dat[ni][nj]='.';nru.push_back(v);break;}
			if(kth==fkth){
				printf("%d",ly*100+lx);
				return 0;
			}
		}
		find=false;
		if(fdl)
			for(int ni=x,nj=y-1;nj>=0;nj--)
				if(dat[ni][nj]=='#'){rem--;find=true;kth++;lx=ni;ly=nj;dat[ni][nj]='.';break;}
		fdl=find;
		if(kth==fkth){
			printf("%d",ly*100+lx);
			return 0;
		}
		for(const point& v:lu){
			find=false;
			if(gcd(-v.x,-v.y)==1)
				for(int ni=x+v.x,nj=y+v.y;ni>=0&&nj>=0;ni+=v.x,nj+=v.y)
					if(dat[ni][nj]=='#'){rem--;find=true;kth++;lx=ni;ly=nj;dat[ni][nj]='.';nru.push_back(v);break;}
			if(kth==fkth){
				printf("%d",ly*100+lx);
				return 0;
			}
		}
		rd=nrd;ru=nru;ld=nld;lu=nlu;
		nrd.clear();nru.clear();nld.clear();nlu.clear();
	}
	return 0;
}
