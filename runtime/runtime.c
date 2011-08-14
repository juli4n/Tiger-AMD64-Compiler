//#undef __STDC__
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gc.h"

#define wSz 8

long* _newArray(long size, long init)
{
	long i;
 	long *a = (long *)GC_MALLOC((size+1)*wSz);
	if (a==NULL) { printf("Runtime error: out of memory.\n");exit(-1);}
	a[0]=size;
	for(i=1;i<(size+1);i++) a[i]=init;
	return a+1;
}

void _checkIndex (long* arr,long index)
{
	if (index==(*(--arr))) {
				printf("\nRuntime error: Indice fuera de rango!\n");
				exit(1);
	}
}
				
long* _newRecord(long size)
{
 long i;
 long *p, *a;
 p = a = (long *) GC_MALLOC(size*wSz);
 if (p==NULL) { printf("Runtime error: out of memory.\n");exit(-1);}
 for(i=0;i<size;i++) *p++ = 0;
 return a;
}

void _checkNil (long p)
{
	if (p==0) { 
		printf("\nRuntime error: Referencia a record nil!\n");
		exit(1);
	}
}

struct string {long length; unsigned char chars[1];}; 

void print(struct string *s)
{
	int i;
	unsigned char* p=(unsigned char*)s->chars;	 
	for(i=0;i<s->length;i++,p++) 
		putchar(*p); 
}

void printint (long d)
{
	printf("%ld",d);
}

long randomint (long range)
{
	long p = (long) (rand() % range);
	return p;
}

void flush()
{
	fflush(stdout);
}

struct string consts[256];
struct string empty={0,""};

int main()
{
 extern int _tigermain (long);
 long i;
 long k=0;
 GC_INIT();
 for(i=0;i<256;i++)
   {consts[i].length=1;
    consts[i].chars[0]=i;
   }
 return _tigermain((long)&k /* static link!? */);
}

long ord(struct string *s)
{
 char j;
 if (s->length==0) return -1;
 else j = s->chars[0];
 return (long) j;
}

struct string *chr(long i)
{
 if (i<0 || i>=256) {
	printf("Runtime error: chr(%ld) out of range\n",i);
	exit(1);
 }
 return consts+i;
}

long size(struct string *s)
{ 
 return s->length;
}


struct string *substring(struct string *s, long first, long n)
{
 if (first<0 || first+n>s->length) {
		printf("Runtime error: substring([%ld],%ld,%ld) out of range\n",s->length,first,n);
    exit(1);
 }
 if (n==1) return consts+s->chars[first];
 {
	struct string *t = (struct string *)GC_MALLOC(wSz+n);
	if (t==NULL) { printf("Runtime error: out of memory.\n");exit(-1);}
  int i;
  t->length=n;
  for(i=0;i<n;i++) t->chars[i]=s->chars[first+i];
  return t;
 }
}


struct string *concat(struct string *a, struct string *b)
{
	if (a->length==0) return b;
 	else if (b->length==0) return a;
 				else {
					long i;
					long n=a->length+b->length;
					struct string *t = (struct string *)GC_MALLOC(wSz+n);
					if (t==NULL) { printf("Runtime error: out of memory.\n");exit(-1);}
					t->length=n;
					for (i=0;i<a->length;i++)
						t->chars[i]=a->chars[i];
					for(i=0;i<b->length;i++)
	 					t->chars[i+a->length]=b->chars[i];
    			return t;
				}
}

enum {EQ,NEQ,LT,LE,GT,GE};

long _compString (long oper, struct string* a, struct string* b)
{
	long res;
	int n;
	char* l = (char*) a->chars;
	char* r = (char*) b->chars;

	n =  (a->length < b->length) ? a->length : b->length;
	
	if (n==0) {
		if (a->length==b->length) res=0;
		else if (a->length==0 && b->length!=0) res=-1;
				 else if (a->length!=0 && b->length==0) res=1;
	}
	else res = strncmp(l,r,n);
	
	switch (oper) {
	case EQ:
			if (res==0) return 1;
			else return 0;
			break;
	case NEQ:
			if (res!=0) return 1;
			else return 0;
			break;
	case LT:
			if (res<0) return 1;
			else return 0;
			break;
	case LE:
			if (res<=0) return 1;
			else return 0;
			break;
	case GT:
			if (res>0) return 1;
			else return 0;
			break;
	case GE:
			if (res>=0) return 1;
			else return 0;
			break;
	}

	printf("Runtime error: error interno en _compString\n");
	exit(-1);
}

long not(long i)
{ 
	return !i;
}

/* #undef getchar */

struct string* getstr()
{
	int i=getc(stdin);
	if (i==EOF)  return &empty;
 	else return consts+i; 
}
