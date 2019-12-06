#include <stdio.h>
int read(){
	int n=0;
	static char ch;
	while(ch!=EOF&&(ch<'0'||ch>'9'))ch=getchar();
	if(ch==EOF)return -1;
	while(ch>='0'&&ch<='9')n=n*10+ch-'0',ch=getchar();
	return n;
}
int values[10005],real[10005];
int main(){
	freopen("Input.txt","r",stdin);
	int i=0;
	while((real[i++]=read())!=-1);
	for(int n=0;n<100;n++)
		for(int v=0;v<100;v++){
			for(int k=0;k<i;k++)
				values[k]=real[k];
			values[1]=n;
			values[2]=v;
			int j=0;
			while(values[j]!=99){
				if(values[j]==1)values[values[j+3]]=values[values[j+2]]+values[values[j+1]];
				else if(values[j]==2)values[values[j+3]]=values[values[j+2]]*values[values[j+1]];
				else return -1;
				j+=4;
			}
			if(values[0]==19690720){
				printf("%d%2d",n,v);
				return 0;
			}
		}
	return -1;
}
