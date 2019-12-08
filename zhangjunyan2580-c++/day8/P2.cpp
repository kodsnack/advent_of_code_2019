#include <stdio.h>
#include <string.h>
char now[6][30];
int noc,ntc;
int main(){
	memset(now,'2',sizeof(now));
	freopen("Input.txt","r",stdin);
	char ch=getchar();
	while(ch!=EOF){
		for(int i=0;i<6;i++)
			for(int j=0;j<25;j++){
				now[i][j]=now[i][j]=='2'?ch:now[i][j];
				ch=getchar();
			}
	}
	for(int i=0;i<6;i++)
		now[i][25]='\0';
	for(int i=0;i<6;i++)
		for(int j=0;j<25;j++)
			if(now[i][j]=='0')
				now[i][j]=' ';
	for(int i=0;i<6;i++)
		printf("%s\n",now[i]);
	return 0;
}
