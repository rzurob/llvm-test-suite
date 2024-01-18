/*  ===================================================================
**  XL Fortran Test Case                          IBM INTERNAL USE ONLY
**  ===================================================================
**
**  TEST CASE TITLE            : C1232arraySect06 - ASYNCHRONOUS
**                               Attribute in Array Section Arguments
**
**  PROGRAMMER                 : Glen Mateer
**  DATE                       : April 10, 2006
**  ORIGIN                     : AIX Compiler Development,
**                             : IBM Software Solutions Toronto Lab
**
**  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section
**  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
**                               Attribute, is an Assumed-Shape Array, and
**                               is passed from a C Function
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
**  C1232 (R1221) If an actual argument is an array section or an
**                assumed-shape array, and the corresponding dummy argument
**                has either the VOLATILE or ASYNCHRONOUS attribute, that
**                dummy argument shall be an assumed-shape array.
**
*234567890123456789012345678901234567890123456789012345678901234567890*/
#include <stdio.h>


int readdata(int *size, float *buffer);


int
csectionreads(int *size, float *buffer)
{
	int ioStat = 0;

	int sect = 0;
	int readSize = 100;

	int section = *size / readSize;
	int floatSize = sizeof( float );

	while ((ioStat == 0)  &&
			(sect < section))
	{
		ioStat = readdata(&readSize, buffer);

		sect++;
		buffer += readSize;
	}

	return( ioStat );
}
