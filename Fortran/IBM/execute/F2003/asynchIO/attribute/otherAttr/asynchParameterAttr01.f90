!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchParameterAttr01 - ASYNCHRONOUS Attribute
!*                               Interactions with Other Attributes
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : February  9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the PARAMETER Attribute
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  5.1 Type declaration statements
!*
!*  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
!*                                      &... :: ] entity-decl-list
!*
!*  R502 declaration-type-spec  is  intrinsic-type-spec
!*
!*  R503 attr-spec  is  access-spec
!*                  or  ALLOCATABLE
!*                  or  ASYNCHRONOUS
!*  ...
!*                  or  PARAMETER
!*
!*
!*  5.2.9 PARAMETER statement
!*
!*  The PARAMETER statement specifies the PARAMETER attribute (5.1.2.10) and
!*  the values for the named constants in the list.
!*
!*  R538 parameter-stmt      is  PARAMETER ( named-constant-def -list )
!*  R539 named-constant-def  is  named-constant = initialization-expr
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchParameterAttr01
    IMPLICIT NONE

    CHARACTER(LEN = 13), PARAMETER, ASYNCHRONOUS :: greeting = 'Hello, World!'
    CHARACTER(LEN = 20), PARAMETER :: salutation = 'Goodbye cruel world.'

    WRITE(6, *) greeting
    WRITE(6, *) salutation

END PROGRAM asynchParameterAttr01
