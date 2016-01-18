#include <cstdlib>
#include <cstdio>
#include <algorithm>
#include <cstring>
#include <vector>
#include <string>
#include <map>
#include <sstream>
#include <iostream>

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

std::vector<std::string> &split(const std::string &s, char delim, std::vector<std::string> &elems) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}


std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, elems);
    return elems;
}

struct Macro
{
	vector<string> args;
	vector<string> code;
};

map<string, Macro> macros;

void assemble(vector<string> file, map<string, string> context)
{
	bool isInMacro = false;
	string macroId="";
	for(int i = 0; i < file.size(); i++)
	{
		char instr[25];
		char as[25],bs[25];
		int a=0,b=0;
		memset(instr, 0, 25);
		memset(as, 0, 25);
		memset(bs, 0, 25);
		int r = sscanf(file[i].c_str(), "%s",instr);

		if(isInMacro)
		{
			if(strstr(instr,"}"))
				isInMacro=false;
			else
				macros[macroId].code.push_back(file[i]);
			continue;
		}

		if(strstr(instr, "(")!=NULL)//new macro
		{
			string mac = "";
			char c=instr[0];
			int j=0;
			while(c!='(')
			{
				mac.push_back(c);
				c=instr[++j];
			}
			j++;
			string args_str = "";
			while(instr[j]!=')')
			{
				if(instr[j]!=' ')
					args_str.push_back(instr[j]);
				j++;
			}
			vector<string> args = split(args_str,',');
			macros[mac]=Macro();
			macros[mac].args = args;
			//macros[mac].code = "";
			isInMacro = true;
			macroId = mac;
		}
		else if(macros.find(string(instr)) != macros.end())
		{
			vector<string> real_args = split(file[i], ' '); //instr arg1 arg2 ...
			map<string, string> context2;
			for(int j = 1; j <= macros[real_args[0]].args.size(); j++)
			{
				if(context.find(real_args[j]) != context.end())
					context2[macros[real_args[0]].args[j-1]]=context[real_args[j]];
				else
					context2[macros[real_args[0]].args[j-1]]=real_args[j];
			}
			assemble(macros[real_args[0]].code, context2);
		}
		else
		{
			sscanf(file[i].c_str(), "%s%d%d",instr,&a,&b); //essaie de lire a et b
			sscanf(file[i].c_str(), "%s%s%s",instr, as, bs);
			string ass = as;
			string bss = bs;
			if(context.find(ass)!=context.end())
				sscanf(context[ass].c_str(), "%d", &a);
			if(context.find(bss)!=context.end())
				sscanf(context[bss].c_str(), "%d", &b);
			//instructions avec un (i) facultatif
			if(strstr(instr, "ADD")!=NULL)
			{
				//fprintf(stderr, "ok : %s et %s\n", instr, ligne);

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
				exit(0);
			}
		}
	}

	putchar('\n');
}

int main(int argc, char* argv[])
{
	size_t sz = 500;
	string line = "";
	if(argc>1)
		freopen(argv[1], "r", stdin);

	vector<string> file;

	while(getline(cin, line))
	{
		file.push_back(line);
	}

	assemble(file, map<string,string>());

	return 0;
}
