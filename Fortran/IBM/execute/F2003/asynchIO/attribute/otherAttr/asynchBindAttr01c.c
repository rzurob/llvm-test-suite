/*  ===================================================================
**  XL Fortran Test Case                          IBM INTERNAL USE ONLY
**  ===================================================================
**
**  TEST CASE TITLE            : asynchBindAttr01 - ASYNCHRONOUS Attribute
**                               Interactions with Other Attributes
**
**  PROGRAMMER                 : Glen Mateer
**  DATE                       : February  8, 2006
**  ORIGIN                     : AIX Compiler Development,
**                             : IBM Software Solutions Toronto Lab
**
**  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
**  SECONDARY FUNCTIONS TESTED : Interactions with the BIND Attribute
**
**  DRIVER STANZA              : xlf2003
**  REQUIRED COMPILER OPTIONS  : -qattr=full
**
**  KEYWORD(S)                 :
**  TARGET(S)                  :
**  NUMBER OF TESTS CONDITIONS : 1
**
**  DESCRIPTION                :
**
**  5.1 Type declaration statements
**
**  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
**                                      &... :: ] entity-decl-list
**
**  R502 declaration-type-spec is intrinsic-type-spec
**
**  R503 attr-spec  is  access-spec
**                  or  ALLOCATABLE
**                  or  ASYNCHRONOUS
**  ...
**                  or  language-binding-spec
**
**
**  5.1.2.4 BIND attribute for data entities
**
**  The BIND attribute for a variable or common block specifies that it is
**  capable of interoperating with a C variable that has external linkage
**  (15.3).
**
**  R509 language-binding-spec  is  BIND (C [, NAME =&
**                                      &scalar-char-initialization-expr ])
**
**
**  5.2.4 BIND statement
**
**  R522 bind-stmt    is  language-binding-spec [ :: ] bind-entity-list
**  R523 bind-entity  is  entity-name
**                    or  / common-block-name /
**
*234567890123456789012345678901234567890123456789012345678901234567890*/

#include <stdio.h>

short cShort;


int
cshortdisp( )
{
	printf("cShort = \"%d\"\n", cShort);

	return 0;
}
