#include <stdio.h>
int main() {
	freopen("Input.txt","r",stdin);
	int n,ans=0;
	while(scanf("%d",&n)==1)ans+=n/3-2;
	printf("%d",ans);
	return 0;
}
