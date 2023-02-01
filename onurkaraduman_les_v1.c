//Lineer Equation Systems Solver V1.00 by DSM
#include <stdio.h>
#define max_eq 4
#define max_eq_size 100
#define max_unknown 3

void trimformula(char *formula);
void find_variables(char *formula, char v_list[max_unknown]);
void find_coefficents(char eq[max_eq][max_eq_size],int coef[max_eq][max_unknown+1],char v1,char v2, char v3);
double determinant(int coef[max_eq][max_unknown+1]);
void copy(int to[max_eq][max_unknown+1], int from[max_eq][max_unknown+1]);
int length(char a[max_eq_size]);
void solve(int coef[max_eq][max_unknown+1],double roots[max_unknown]);
int unk;

int main(void){

	int eq_coef[max_eq][max_unknown+1]={0},i,j,k,sumcoef=0,eq_count,reelflag=0;
	char v_list[10]={0},equations[max_eq][max_eq_size]={0},v1,v2,v3,v4,v5;
	double root[max_unknown];
			
	printf("\n\t\t\t********************* Welcome to Linear Equation Solver *********************\n");
	printf("\nPlease enter your equations [Q]: \n");

	for(i=0;i<max_eq;i++){
		fgets(equations[i],sizeof(equations[i])%(max_eq_size+1),stdin); 
	 	if(equations[i][0]=='q'||equations[i][0]=='Q'){
	 		if(i==0) return 0;
	 		eq_count=i;
	 		if(i<length(v_list)){
	 			printf("The number of equations cannot be less than the number of unknowns.");
	 			getch();
	 			system("cls");
	 			return main();
			}
			break;
		}
		
		trimformula(equations[i]);
		find_variables(equations[i],v_list);
		if(length(v_list)>max_unknown){
			printf("Too Many Variables!\nYour Variables: %s",v_list);
			getch();
			system("cls");
			return main();
		}
		unk=length(v_list);
		v1=v_list[0],v2=v_list[1],v3=v_list[2],v4=v_list[3],v5=v_list[4];
	}
	
	find_coefficents(equations,eq_coef,v1,v2,v3);
	for(i=0;i<unk;i++){
		for(j=0;j<eq_count;j++)
			sumcoef+=eq_coef[j][i];
		if(sumcoef){ 
			reelflag=0;
			break;}
		else reelflag=1;
		sumcoef=0;
	}	
/*	for(i=0;i<eq_count;i++)  // katsayilarin tutuldugu  arrayin içini görmek için
		printf("  %c%d= %-5d  %c%d= %-5d  %c%d= %-5d  const%d= %-5d \n",v1,i+1,eq_coef[i][0],v2,i+1,eq_coef[i][1],v3,i+1,eq_coef[i][2],i+1,eq_coef[i][3]);*/

	if(reelflag) printf("\nThe answer is = {R}");
	else if(unk>1)solve(eq_coef,root);
	else if (unk==1) root[0]=(double)eq_coef[0][3]/(double) eq_coef[0][0];
	if(!reelflag) 
		for(i=0;i<unk;i++)
			printf("\n%c = %.4lf",v_list[i],root[i]);
}


void trimformula(char *formula){
	char *i;
	for(;*formula!='\0';formula++)
		if(*formula==' '||*formula=='\n')
			for(i=formula;*i!='\0';i++)
				*i=*(i+1);
}


void find_variables(char *formula, char v_list[10]){
	int vlist_flag=1,j;
	int static k=0;
	char *i=formula;	
	
	for(;*i!='\0';i++){
		if(isalpha(*i)){ // karakter kontrolü
			for(j=0;v_list[j]!='\0';j++)
				if(*i==v_list[j]) vlist_flag=0;
			if(vlist_flag){
				v_list[k]=*i;	
				k++;}
			vlist_flag=1;
		}		
	}
}

void find_coefficents(char eq[max_eq][max_eq_size],int coef[max_eq][max_unknown+1],char v1,char v2, char v3){
	int j;
	for(j=0;j<max_eq;j++){
		int n_flag=0,eq_const=1,sum=0,i=0,sumv1=0,sumv2=0,sumv3=0,sumnum=0;
		for(i=0;eq[j][i]!='\0';i++){
			if(eq[j][i]=='-'){
				n_flag=1;
				i++;}
			while(isdigit(eq[j][i])){
				sum=10*sum+(eq[j][i]-'0');
				i++;}
			
			if(eq[j][i]=='-') i--;
			if((eq[j][i]==v1||eq[j][i]==v2||eq[j][i]==v3)&&sum==0&&isalpha(eq[j][i])) sum=1;
			if(n_flag){
				sum*=-1;
				n_flag=0;}
			if(eq[j][i]==v1&&isalpha(v1)) sumv1+=sum*eq_const;
			else if(eq[j][i]==v2&&isalpha(v2)) sumv2+=sum*eq_const;
			else if(eq[j][i]==v3&&isalpha(v3))sumv3+=sum*eq_const; 
			else sumnum+=sum;
			sum=0;
 			
			if(eq[j][i]=='='){
				sumnum*=-1;
				eq_const=-1;}		
		}
		coef[j][0]=sumv1;
		coef[j][1]=sumv2;
		coef[j][2]=sumv3;
		coef[j][3]=sumnum;
	}
}


double determinant(int coef[max_eq][max_unknown+1]){
	int i,j,lim=3;
	double det=1,sum=0;
	 	 	 
	if(unk==2)
		lim=1;	
	
	for(i=0;i<lim;i++){
		for(j=0;j<unk;j++)
			det*=coef[(i+j)%unk][j];  
		sum+=det;						
		det=1;
	}
	for(i=0;i<lim;i++){
		for(j=unk-1;j>=0;j--)
			det*=coef[(i+unk-1-j)%unk][j];
		sum-=det;
		det=1;
	}
	return sum;
}


void copy(int to[max_eq][max_unknown+1], int from[max_eq][max_unknown+1]){
	int i,j;
	for(i=0;i<max_eq;i++)
		for(j=0;j<max_unknown+1;j++)
			to[i][j]=from[i][j];
} 

int length(char a[max_eq_size]){
	int i=0; 
	for(;isalpha(a[i]);i++);
	return i;
}

void solve(int coef[max_eq][max_unknown+1],double roots[max_unknown]){
	int i,j;
	int temp[max_eq][max_unknown+1]={0};
	double det_A=determinant(coef);
	double det_temp;
	for(j=0;j<unk;j++){
		copy(temp,coef);			
		for(i=0;i<max_eq;i++)	
			temp[i][j]=temp[i][3];
		det_temp=determinant(temp);
		roots[j]=det_temp/det_A;
	}
}
