#include <stdio.h>
int count,l,r,ls[10];
int main(){
	freopen("Input.txt","r",stdin);
	scanf("%d-%d",&l,&r);
	for(;l<=r;l++){
		bool flag=false;
		for(int i=5,lw=l;i>=0;i--){ls[i]=lw%10;lw/=10;}
		for(int i=0;i<5;i++)if(ls[i]>ls[i+1]){flag=true;break;}
		if(flag)continue;
		int cw=0;
		for(int i=0;i<5;i++){
			if(ls[i]==ls[i+1])cw++;
			else if(cw==1){flag=true;break;}
			else cw=0;
		}
		if(cw==1||flag){
			printf("%d\n",l);
			count++;
		}
	}
	printf("%d",count);
	return 0;
}
