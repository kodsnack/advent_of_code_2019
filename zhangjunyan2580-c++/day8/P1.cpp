#include <stdio.h>
int mzc=1000000000,moc,mtc,nzc,noc,ntc;
int main(){
	freopen("Input.txt","r",stdin);
	char ch=getchar();
	while(ch!=EOF){
		for(int i=0;i<150;i++){
			if(ch=='0')nzc++;
			else if(ch=='1')noc++;
			else if(ch=='2')ntc++;
			ch=getchar();
		}
		if(mzc>nzc)moc=noc,mtc=ntc,mzc=nzc;
		noc=nzc=ntc=0;
	} 
	printf("%d",mtc*moc);
	return 0;
}
