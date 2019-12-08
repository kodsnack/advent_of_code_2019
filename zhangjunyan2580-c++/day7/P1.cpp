#include <stdio.h>
int isEOF,sz,code[10005],sett[6],used[6],ans;
struct program{
	int code[10005],index;
	int get_para(int ind,int mode){
		if(mode==0)
			return code[ind];
		else
			return ind;
	}
	void mod_para(int ind,int val){
		code[ind]=val;
	}
	void init(){
		for(int i=0;i<sz;i++)
			code[i]= ::code[i];
	}
	int run(int input[2]){
		index=0;
		int value,iid=0;
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
					mod_para(code[index+1],input[iid]);
					iid++;
					index+=2;
					break;
				case 4:
					value=get_para(code[index+1],amode);
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
					return value;
					break;
			}
		}
		return value;
	}
}pro;
int read(){
	static char ch;
	int n=0,f=1;
	while(ch!=EOF&&ch!='-'&&(ch<'0'||ch>'9'))ch=getchar();
	if(ch==EOF){isEOF=1;return 0;}
	if(ch=='-')f=-1,ch=getchar();
	while(ch>='0'&&ch<='9')n=n*10+ch-'0',ch=getchar();
	return n*f;
}
void dfs(int t){
	if(t==5){
		int last=0;
		for(int i=0;i<5;i++){
			pro.init();
			int inp[2]={sett[i],last};
			last=pro.run(inp);
		}
		if(last>ans)ans=last;
	}else
		for(int i=0;i<5;i++)
			if(!used[i]){
				used[i]=1;
				sett[t]=i;
				dfs(t+1);
				used[i]=0;
			}
}
int main(){
	freopen("Input.txt","r",stdin);
	sz=1;code[0]=read();
	while(!isEOF)code[sz++]=read();
	dfs(0);
	printf("%d",ans);
	return 0;
}
