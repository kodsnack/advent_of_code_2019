#include <stdio.h>
#include <string.h>
int n,m,a,ans;
char dat[105][105];
int gcd(int a,int b){return b?gcd(b,a%b):a;}
int main(){
	freopen("Input.txt","r",stdin);
	while(scanf(" %s",dat[n])==1)n++;
	m=strlen(dat[0]);
	for(int i=0;i<n;i++)
		for(int j=0;j<m;j++)
			if(dat[i][j]=='#'){
				a=0;
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
				if(a>ans)ans=a;
			}
	printf("%d",ans);
	return 0;
}
