%{

#include<stdio.h>

void
printyy();

%}

%option noyywrap

float	float
other	[a-zA-Z0-9 \n]

%%

{float}	{printf("double");}
{other}	{printyy();}

%%

void
printyy()
{
	printf("%.*s", (int) yyleng, yytext);
}

/* read_file: reads contents of file and returns them
 * caller must free returned string 
 * see https://stackoverflow.com/a/14002993 */
char *
read_file(char *path)
{
	FILE *f = fopen(path, "rb");
	fseek(f, 0, SEEK_END);
	long fsize = ftell(f);
	fseek(f, 0, SEEK_SET);  /* same as rewind(f); */
	char *str = malloc(fsize + 1);
	fread(str, fsize, 1, f);
	fclose(f);
	str[fsize] = '\0';
	return str;
}

int
main(int argc, char *argv[])
{
	if (argc != 2) {
		perror("must supply input file");
		exit(1);
	}
	char *infile = read_file(argv[1]);
	yy_scan_string(infile);
	free(infile);
	yylex(); 
}
