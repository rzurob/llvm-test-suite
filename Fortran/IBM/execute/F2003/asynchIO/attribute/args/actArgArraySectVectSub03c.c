/*  ===================================================================
**  XL Fortran Test Case                          IBM INTERNAL USE ONLY
**  ===================================================================
**
**  TEST CASE TITLE            : actArgArraySectVectSub03c - ASYNCHRONOUS
**                               Attribute in Array Section Arguments
**
**  PROGRAMMER                 : Glen Mateer
**  DATE                       : April 10, 2006
**  ORIGIN                     : AIX Compiler Development,
**                             : IBM Software Solutions Toronto Lab
**
**  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section (with a
**                               Vector Subscript)
**  SECONDARY FUNCTIONS TESTED : Dummy Argument Specification is for a C
**                               function and does *NOT* explicitly include
**                               the ASYNCHRONOUS Attribute
**
**  DRIVER STANZA              : xlf2003
**  REQUIRED COMPILER OPTIONS  :
**
**  KEYWORD(S)                 : ASYNCHRONOUS Attribute
**  TARGET(S)                  :
**  NUMBER OF TESTS CONDITIONS : 1
**
**  DESCRIPTION                :
**
**  12.4.1.2 Actual arguments associated with dummy data objects
**
**  If the actual argument is an array section having a vector subscript,
**  the dummy argument is not definable and shall not have the INTENT (OUT),
**  INTENT (INOUT), VOLATILE, or ASYNCHRONOUS attributes.
**
*234567890123456789012345678901234567890123456789012345678901234567890 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>


static int fileHandle;


int
copen( void )
{
	int ioStat = 0;


	fileHandle =
		open("actArgArraySectVectSub03.dat",
				(O_WRONLY | O_CREAT | O_TRUNC),
				(S_IRUSR | S_IWUSR | S_IRGRP ));
	if (fileHandle < 0)
	{
		perror( "COpen" );
		ioStat = fileHandle;
	}


	return( ioStat );
}


int
cwritedata(int *size, float *dataArray)
{
	int i;
	int num;
	int ioStat = 0;
	int writeSize = *size * sizeof( float );


	num = write(fileHandle, (void *)dataArray, writeSize);
	if (num != writeSize)
	{
		perror( "CWriteData" );
		ioStat = num - writeSize;
	}


	return( ioStat );
}


int
cclose( void )
{
	int ioStat = close( fileHandle );
	if (ioStat != 0)
	{
		perror( "CClose" );
	}

	return( ioStat );
}
