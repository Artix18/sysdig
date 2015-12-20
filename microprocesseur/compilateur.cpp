#include <cstdlib>
#include <cstdio>
#include <algorithm>
#include <cstring>

using namespace std;

//note : j'ai choisi de faire un truc où il est facile d'ajouter d'autres cas ou modifier ceux présents, quitte à ce que
//       ce ne soit pas très beau.

const int OUTPUT_SZ=27;
const int TAILLE_REGISTRE=5; //0..31
const int SMALL_SZ=16;
const int WORD_SZ=32;

const int NB_INSTR = 12;
enum INSTR_TYPE
{
	ADD=0,
	SUB=1,
	MOVE=2,
	AND=3,
	OR=4,
	XOR=5,
	NOT=6,
	LSE=7,
	RSE=8,
	EQZ=9,
	LTZ=10,
	MTZ=11
};

const char *code[NB_INSTR] =
{ 
	"0000", //ADD
	"0001", //SUB
	"0010", //MOVE
	"0011", //AND
	"0100", //OR
	"0101", //XOR
	"0110", //NOT
	"0111", //LSE
	"1000", //RSE
	"1001", //EQZ
	"1010", //LTZ
	"1011", //MTZ
};

void affiche_instr(INSTR_TYPE instr)
{
	printf("%s",code[instr]);
}

void affiche_zero(int nb)
{
	for(int i = 0; i < nb; i++)
		printf("0");
}

void affiche_binaire(int nb, int sz)
{
	if(nb==0)
	{
		affiche_zero(sz);
		return;
	}
	
	affiche_binaire(nb/2, sz-1);
	printf("%d", nb&1);
}

int main(int argc, char* argv[])
{
	size_t sz = 500;
	char *ligne = (char*)malloc(sz+1);
	if(argc>1)
		freopen(argv[1], "r", stdin);
	
	int i=0;
	while(getline(&ligne, (&sz), stdin)!=-1)
	{
		i++;
		char instr[25];
		int a=0,b=0;
		memset(instr, 0, 25);
		int r = sscanf(ligne, "%s%d%d",instr, &a,&b);
		
		//instructions avec un (i) facultatif
		if(strstr(instr, "ADD")!=NULL)
		{
			fprintf(stderr, "ok : %s et %s\n", instr, ligne);
			
			//4 bits
			affiche_instr(ADD);

			//1 bit
			int c=(instr[3]=='i');
			printf("%d", c);
			
			//les registres sont numérotés à partir de 1
			b--;
			if(!c)
				a--;
			
			//taille du premier truc
			int sz_prems = c?SMALL_SZ:TAILLE_REGISTRE;

			affiche_binaire(a,sz_prems);
			affiche_binaire(b,TAILLE_REGISTRE);
			
			//complète jusqu'à 27 bits
			affiche_zero(OUTPUT_SZ-(4+1+sz_prems+TAILLE_REGISTRE));
		}
		else if(strstr(instr, "SUB")!=NULL)
		{
			//pareil que ADD
			
			//4 bits
			affiche_instr(SUB);

			//1 bit
			int c=(instr[3]=='i');
			printf("%d", c);
			
			//les registres sont numérotés à partir de 1
			b--;
			if(!c)
				a--;
			
			//taille du premier truc
			int sz_prems = c?SMALL_SZ:TAILLE_REGISTRE;

			affiche_binaire(a,sz_prems);
			affiche_binaire(b,TAILLE_REGISTRE);
			
			//complète jusqu'à 27 bits
			affiche_zero(OUTPUT_SZ-(4+1+sz_prems+TAILLE_REGISTRE));
		}
		else if(strstr(instr, "MOVE")!=NULL)
		{
			//TODO
			affiche_instr(MOVE);
		}
		else if(strstr(instr, "EQZ")!=NULL)
		{
			affiche_instr(EQZ);
			//astuce de C : b n'a pas été lu
			
			int c = (instr[3]=='i');
			printf("%d", c);
			
			int sz_prems=0;
			if(!c)
			{
				//a est un registre, numéroté à partir de 1
				a--;
				sz_prems = TAILLE_REGISTRE;
			}
			else
				sz_prems = SMALL_SZ;
			
			affiche_binaire(a,sz_prems);
			
			//complète
			affiche_zero(OUTPUT_SZ - (4+1+sz_prems));
		}
		else if(strstr(instr, "LTZ")!=NULL)
		{
			affiche_instr(LTZ);
			//astuce de C : b n'a pas été lu
			
			int c = (instr[3]=='i');
			printf("%d", c);
			
			int sz_prems=0;
			if(!c)
			{
				//a est un registre, numéroté à partir de 1
				a--;
				sz_prems = TAILLE_REGISTRE;
			}
			else
				sz_prems = SMALL_SZ;
			
			affiche_binaire(a,sz_prems);
			
			//complète
			affiche_zero(OUTPUT_SZ - (4+1+sz_prems));
		}
		else if(strstr(instr, "MTZ")!=NULL)
		{
			affiche_instr(MTZ);
			//astuce de C : b n'a pas été lu
			
			int c = (instr[3]=='i');
			printf("%d", c);
			
			int sz_prems=0;
			if(!c)
			{
				//a est un registre, numéroté à partir de 1
				a--;
				sz_prems = TAILLE_REGISTRE;
			}
			else
				sz_prems = SMALL_SZ;
			
			affiche_binaire(a,sz_prems);
			
			//complète
			affiche_zero(OUTPUT_SZ - (4+1+sz_prems));
		}
		//fin des instructions (i)
		else if(strcmp(instr, "AND")==0)
		{
			affiche_instr(AND);
			
			//i=0
			printf("0");
			
			//registres indexés à partir de 1
			a--,b--;
			
			affiche_binaire(a,TAILLE_REGISTRE);
			affiche_binaire(b,TAILLE_REGISTRE);
			
			//complète
			affiche_zero(OUTPUT_SZ - (4+1+TAILLE_REGISTRE+TAILLE_REGISTRE));
		}
		else if(strcmp(instr,"OR")==0)
		{
			affiche_instr(OR);
			
			//i=0
			printf("0");
			
			//registres indexés à partir de 1
			a--,b--;
			
			affiche_binaire(a,TAILLE_REGISTRE);
			affiche_binaire(b,TAILLE_REGISTRE);
			
			//complète
			affiche_zero(OUTPUT_SZ - (4+1+TAILLE_REGISTRE+TAILLE_REGISTRE));
		}
		else if(strcmp(instr,"XOR")==0)
		{
			affiche_instr(XOR);
			
			//i=0
			printf("0");
			
			//registres indexés à partir de 1
			a--,b--;
			
			affiche_binaire(a,TAILLE_REGISTRE);
			affiche_binaire(b,TAILLE_REGISTRE);
			
			//complète
			affiche_zero(OUTPUT_SZ - (4+1+TAILLE_REGISTRE+TAILLE_REGISTRE));
		}
		else if(strcmp(instr,"NOT")==0)
		{
			affiche_instr(NOT);
			
			//i=0
			printf("0");
			
			//registres indexés à partir de 1
			a--,b--;
			
			affiche_binaire(a,TAILLE_REGISTRE);
			affiche_binaire(b,TAILLE_REGISTRE);
			
			//complète
			affiche_zero(OUTPUT_SZ - (4+1+TAILLE_REGISTRE+TAILLE_REGISTRE));
		}
		else if(strcmp(instr,"LSE")==0)
		{
			affiche_instr(LSE);
			
			//i=0
			printf("0");
			
			//astuce de C : b n'a pas été lu
			//a est un registre, numéroté à partir de 1
			a--;
			
			affiche_binaire(a,TAILLE_REGISTRE);
			
			//complète
			affiche_zero(OUTPUT_SZ - (4+1+TAILLE_REGISTRE));
		}
		else if(strcmp(instr,"RSE")==0)
		{
			affiche_instr(RSE);
			
			//i=0
			printf("0");
			
			//astuce de C : b n'a pas été lu
			//a est un registre, numéroté à partir de 1
			a--;
			
			affiche_binaire(a,TAILLE_REGISTRE);
			
			//complète
			affiche_zero(OUTPUT_SZ - (4+1+TAILLE_REGISTRE));
		}
		else
		{
			fprintf(stderr, "FATAL ERROR : instruction non reconnue, ligne %d.\n",i);
			return 0;
		}
	}
	
	putchar('\n');
	
	return 0;
}
