#include <stdio.h>
int count,l,r;
int main(){
	freopen("Input.txt","r",stdin);
	scanf("%d-%d",&l,&r);
	for(;l<=r;l++){
		int lw=l/10,lastd=l%10;
		bool flag=true,adfl=false;
		while(lw){
			if(lw%10>lastd){flag=false;break;}
			if(lw%10==lastd)adfl=true;
			lastd=lw%10;lw/=10;
		}
		if(flag&&adfl)count++;
	}
	printf("%d",count);
	return 0;
} 
