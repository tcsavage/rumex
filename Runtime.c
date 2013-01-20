#import <stdio.h>
#import <stdlib.h>

#define TAPE_SIZE 5000

unsigned char * tape = 0;
unsigned char * ptr = 0;

void init()
{
	tape = (char*)malloc(TAPE_SIZE);
	ptr = tape;
}

void shutdown()
{
	free(tape);
}

void opInc()
{
	++*ptr;
}

void opDec()
{
	--*ptr;
}

void opRight()
{
	++ptr;
}

void opLeft()
{
	--ptr;
}

int main(int argc, char * argv[])
{
	init();

	shutdown();
}
