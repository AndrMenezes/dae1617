   title 'Balanced Data from Randomized Complete Block';
   data plants;
      input Type $ @;
      do Block = 1 to 3;
         input StemLength @;
         output;
         end;
      datalines;
   Clarion  32.7 32.3 31.5
   Clinton  32.1 29.7 29.1
   Knox     35.7 35.9 33.1
   O'Neill  36.0 34.2 31.2
   Compost  31.8 28.0 29.2
   Wabash   38.2 37.8 31.9
   Webster  32.5 31.1 29.7
   ;

/*Dois fatores influenciando a resposta.*/
proc glm;
   class Block Type;
   model StemLength = Block Type;
quit;

/*Unico fator influenciando a resposta.*/
proc glm;
   class Type;
   model StemLength = Type;
quit;

/*Unico fator influenciando a resposta.*/
proc glm;
   class Type;
   model StemLength = Type;
quit;


/*Teste t-bonferroni*/
proc glm;
	class type;
	model stemlength = type;
	means Type / bon cldiff;
   	lsmeans Type / adjust=bon;
quit;

/*Teste de Duncan*/
proc glm data=plants;
	class type;
	model stemlength = type;
	means Type / duncan lines ;
quit;


data plantgrowth;
	input id weight group$;
	cards;
1    4.17  ctrl
2    5.58  ctrl
3    5.18  ctrl
4    6.11  ctrl
5    4.50  ctrl
6    4.61  ctrl
7    5.17  ctrl
8    4.53  ctrl
9    5.33  ctrl
10   5.14  ctrl
11   4.81  trt1
12   4.17  trt1
13   4.41  trt1
14   3.59  trt1
15   5.87  trt1
16   3.83  trt1
17   6.03  trt1
18   4.89  trt1
19   4.32  trt1
20   4.69  trt1
21   6.31  trt2
22   5.12  trt2
23   5.54  trt2
24   5.50  trt2
25   5.37  trt2
26   5.29  trt2
27   4.92  trt2
28   6.15  trt2
29   5.80  trt2
30   5.26  trt2
;


/*Teste t-bonferroni*/
proc glm data=plantgrowth;
	class group;
	model weight = group;
	means group / bon cldiff;
   	lsmeans group / adjust=bon;
quit;

/*Teste de Duncan*/
proc glm data=plantgrowth;
	class group;
	model weight = group;
	means group / duncan;
quit;

/*Teste de scheffe*/
proc glm data=plantgrowth;
	class group;
	model weight = group;
	means group / scheffe cldiff;
quit;


filename arq "C:\Users\André Felipe\Dropbox\UEM\3° Série\Planejamento e Análise de Experimentos I\Trabalho - Testes (Duncan Bonferoni e Dunnet)\Datasets\chopstick-effectiveness.csv";
data chopstick;
	infile arq dlm="," firstobs=2;
	input Food Individual ChopstickLength;
run;
/*Teste t-bonferroni*/
proc glm data=chopstick;
	class chopsticklength;
	model food = chopsticklength;
*	means chopsticklength / bon cldiff;
   	lsmeans chopsticklength / adjust=bon;
quit;

/*Teste de Duncan*/
proc glm data=chopstick;
	class chopsticklength;
	model food = chopsticklength;
	means chopsticklength / duncan ;
quit;


data Cholesterol;
do Trt = '1time','2times','4times','drugD','drugE';
do i = 1 to 10;
input Response @@;
output;
end;
end;
datalines;
3.86 10.39 5.91 3.06 7.72 2.71 4.92 2.30 7.53 9.41
10.40 8.60 13.63 3.51 7.77 8.63 9.23 6.32 15.83 8.34
13.96 13.96 13.92 8.05 11.04 12.37 10.39 9.03 12.84 18.18
16.98 15.46 19.98 14.74 13.59 10.86 17.59 8.82 17.96 17.63
21.51 27.24 20.52 15.77 22.89 23.95 21.59 18.31 20.39 17.31
;
proc glm data=Cholesterol;
	class Trt;
	model Response=Trt;
	lsmeans Trt / adjust=bon plot=diff;
	lsmeans Trt / adjust=dunnett diff=control("D") plot=diff pdiff;
*	means Trt / duncan lines;
quit;
