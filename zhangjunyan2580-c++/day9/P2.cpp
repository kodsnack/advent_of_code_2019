#include <vector>
#include <algorithm>
using namespace std;
struct bignum{
	vector<int> di;
	bool sign;
	bignum(int num=0){
		sign=false;
		if(num==0)return;
		if(num<0){
			num=-num;
			sign=true;
		}
		while(num){
			di.push_back(num%10);
			num/=10;
		}
	}
	bignum operator-()const{
		bignum res=*this;
		res.sign=!sign;
		return res;
	}
	operator int()const{
		int ret=0;
		for(int i=di.size()-1;i>=0;i--)
			ret=ret*10+di[i];
		return sign?-ret:ret;
	}
	bool operator<(const bignum& num)const{
		if(sign&&!num.sign)return false;
		if(!sign&&num.sign)return true;
		if(di.size()!=num.di.size())
			return (di.size()<num.di.size())^sign;
		for(int i=di.size()-1;i>=0;i--)
			if(di[i]!=num.di[i])
				return (di[i]<num.di[i])^sign;
		return false;
	}
	bool operator==(const bignum& num)const{
		if(sign^num.sign)return false;
		if(di.size()!=num.di.size())
			return false;
		for(int i=0;i<di.size();i++)
			if(di[i]!=num.di[i])
				return false;
		return true;
	}
	bignum minus_p(const bignum& num)const{
		bignum res;
		int i=0;
		for(;i<num.di.size();i++)
			res.di.push_back(di[i]-num.di[i]);
		for(;i<di.size();i++)
			res.di.push_back(di[i]);
		for(i=1;i<res.di.size();i++)
			if(res.di[i-1]<0){
				res.di[i-1]+=10;
				res.di[i]--;
			}
		while(!res.di.empty()&&!res.di.back())res.di.pop_back();
		return res;
	}
	bignum plus_p(const bignum& num)const{
		bignum res;
		int i=0;
		for(;i<di.size()&&i<num.di.size();i++)
			res.di.push_back(di[i]+num.di[i]);
		for(;i<di.size();i++)
			res.di.push_back(di[i]);
		for(;i<num.di.size();i++)
			res.di.push_back(num.di[i]);
		for(i=1;i<res.di.size();i++)
			if(res.di[i-1]>9){
				res.di[i-1]-=10;
				res.di[i]++;
			}
		if(res.di.back()>9){
			res.di.back()-=10;
			res.di.push_back(1);
		}
		return res;
	}
	bignum operator+(const bignum& num)const{
		if(sign)
			if(num.sign)
				return operator-().plus_p(-num);
			else
				return num.minus_p(operator-());
		else
			if(num.sign)
				return minus_p(-num);
			else
				return plus_p(num);
	}
	bignum operator*(const bignum& num)const{
		bignum res;
		res.di.resize(di.size()+num.di.size(),0);
		for(int i=0;i<di.size();i++)
			for(int j=0;j<num.di.size();j++)
				res.di[i+j]+=di[i]*num.di[j];
		for(int i=1;i<res.di.size();i++){
			res.di[i]+=res.di[i-1]/10;
			res.di[i-1]%=10;
		}
		if(!res.di.empty()&&!res.di.back())res.di.pop_back();
		res.sign=sign^num.sign;
		return res;
	}
	bool read(){
		static char ch;
		di.clear();
		while(ch!=EOF&&ch!='-'&&(ch<'0'||ch>'9'))ch=getchar();
		if(ch==EOF)return false;
		if(ch=='-')sign=1,ch=getchar();else sign=0;
		while(ch!=EOF&&(ch>='0'&&ch<='9'))di.push_back(ch-'0'),ch=getchar();
		reverse(di.begin(),di.end());
		return true;
	}
	void print()const{
		if(di.size()==0)putchar('0');
		if(sign)putchar('-');
		for(int i=di.size()-1;i>=0;i--)
			putchar(di[i]+'0');
	}
};
bignum code[100005];
int sz;
struct program{
	bignum code[100005];
	int index,relind,sz;
	bignum get_para(bignum ind,int mode){
		if(mode==0)
			return code[ind];
		else if(mode==2)
			return code[ind.operator int()+relind];
		else
			return ind;
	}
	void mod_para(bignum ind,bignum val,int mode){
		if(mode==0)
			code[ind]=val;
		else if(mode==2)
			code[ind.operator int()+relind]=val;
	}
	void init(int sz){
		this->sz=sz;
		for(int i=0;i<sz;i++)
			code[i]= ::code[i];
	}
	vector<bignum> run(const vector<bignum>& input){
		index=0;
		int iid=0;
		vector<bignum> output;
		while(index<sz){
			int opcode=code[index]%100,
				amode=code[index]/100%10,
				bmode=code[index]/1000%10,
				cmode=code[index]/10000;
			switch(opcode){
				case 1:
					mod_para(code[index+3],get_para(code[index+2],bmode)+get_para(code[index+1],amode),cmode);
					index+=4;
					break;
				case 2:
					mod_para(code[index+3],get_para(code[index+2],bmode)*get_para(code[index+1],amode),cmode);
					index+=4;
					break;
				case 3:
					mod_para(code[index+1],input[iid],amode);
					iid++;
					index+=2;
					break;
				case 4:
					output.push_back(get_para(code[index+1],amode));
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
					mod_para(code[index+3],get_para(code[index+1],amode)<get_para(code[index+2],bmode),cmode);
					index+=4;
					break;
				case 8:
					mod_para(code[index+3],get_para(code[index+1],amode)==get_para(code[index+2],bmode),cmode);
					index+=4;
					break; 
				case 9:
					relind+=get_para(code[index+1],amode);
					index+=2;
					break;
				case 99:
					return output;
					break;
			}
		}
		return output;
	}
}pro;
int main(){
	freopen("Input.txt","r",stdin);
	while(code[sz].read())sz++;
	pro.init(sz);
	vector<bignum> val=pro.run({2});
	for(const bignum& v:val){
		v.print();
		putchar(' ');
	}
	return 0;
}
