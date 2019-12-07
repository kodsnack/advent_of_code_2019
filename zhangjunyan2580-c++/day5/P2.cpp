#include <stdio.h>
#include <stdlib.h>
int isEOF=0,code[10005],i,index;
int get_para(int ind,int mode){
	if(mode==0)
		return code[ind];
	else
		return ind;
}
void mod_para(int ind,int val){
	code[ind]=val;
}
int read(){
	static char ch;
	int n=0,f=1;
	while(ch!=EOF&&ch!='-'&&(ch<'0'||ch>'9'))ch=getchar();
	if(ch==EOF){isEOF=1;return 0;}
	if(ch=='-')f=-1,ch=getchar();
	while(ch>='0'&&ch<='9')n=n*10+ch-'0',ch=getchar();
	return n*f;
}
int main(){
	freopen("Input.txt","r",stdin);
	i=1;code[0]=read();
	while(!isEOF)code[i++]=read();
	while(code[index]%100!=99){
		int opcode=code[index]%100,
			amode=code[index]/100%10,
			bmode=code[index]/1000%10;
		switch(opcode){
			case 1:
				mod_para(code[index+3],get_para(code[index+2],bmode)+get_para(code[index+1],amode));
				index+=4;
				break;
			case 2:
				mod_para(code[index+3],get_para(code[index+2],bmode)*get_para(code[index+1],amode));
				index+=4;
				break;
			case 3:
				mod_para(code[index+1],5);
				index+=2;
				break;
			case 4:
				printf("%d ",get_para(code[index+1],amode));
				index+=2;
				break;
			case 5:
				if(get_para(code[index+1],amode))
					index=get_para(code[index+2],bmode);
				else
					index+=3;
				break;
			case 6:
				if(!get_para(code[index+1],amode))
					index=get_para(code[index+2],bmode);
				else
					index+=3;
				break;
			case 7:
				mod_para(code[index+3],get_para(code[index+1],amode)<get_para(code[index+2],bmode));
				index+=4;
				break;
			case 8:
				mod_para(code[index+3],get_para(code[index+1],amode)==get_para(code[index+2],bmode));
				index+=4;
				break; 
			case 99:
				exit(0);
				break;
		}
	}
	return 0;
}
