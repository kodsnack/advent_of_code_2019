#include <stdio.h>
int read(){
	int n=0;
	static char ch;
	while(ch!=EOF&&(ch<'0'||ch>'9'))ch=getchar();
	if(ch==EOF)return -1;
	while(ch>='0'&&ch<='9')n=n*10+ch-'0',ch=getchar();
	return n; 
}
int values[100005];
int main(){
	freopen("Input.txt","r",stdin);
	int i=0,j=0;
	while((values[i++]=read())!=-1);
	values[1]=12;
	values[2]=2;
	while(values[j]!=99){
		if(values[j]==1)values[values[j+3]]=values[values[j+2]]+values[values[j+1]];
		else if(values[j]==2)values[values[j+3]]=values[values[j+2]]*values[values[j+1]];
		else return -1;
		j+=4;
	}
	printf("%d",values[0]);
	return 0;
}
