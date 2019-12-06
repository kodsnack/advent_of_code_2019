#include <stdio.h>
int main(){
	freopen("Input.txt","r",stdin);
	int n,ans=0;
	while(scanf("%d",&n)==1)
		while(n>0)ans+=((n=(n/3-2))>0?n:0);
	printf("%d",ans);
	return 0;
}
