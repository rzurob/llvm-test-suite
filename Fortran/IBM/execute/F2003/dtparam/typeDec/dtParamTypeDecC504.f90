!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 07, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* 
!*  If a type-param-value in a char-length in an entity-decl is not a colon or
!*  an asterisk, it shall be a specification-expr.
!*  
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC501

  
  IF ( SIZE( IntFun(10) ) .NE. 10 ) STOP 11
  IF ( LEN ( IntFun(10) ) .NE. 10 ) STOP 12
  IF ( ANY ( IntFun(10)   .NE. REPEAT( CHAR(10), 10) )) STOP 13

  CONTAINS
  FUNCTION IntFun(Arg)
  INTEGER          :: Arg
  CHARACTER        :: C*(Arg)
  CHARACTER(Arg-1) :: C1*(Arg)
  CHARACTER        :: C2(Arg)*(Arg)
  CHARACTER(Arg+1) :: C3(Arg)*(Arg)
  CHARACTER(Arg-1) :: IntFun(Arg)*(Arg)
  
  IF ( LEN ( C )  .NE. Arg  ) STOP 21

  IF ( LEN ( C1 ) .NE. Arg ) STOP 31

  IF ( LEN ( C2 ) .NE. Arg ) STOP 41
  IF ( SIZE( C2 ) .NE. Arg ) STOP 42

  IF ( LEN ( C3 ) .NE. Arg ) STOP 51
  IF ( SIZE( C3 ) .NE. Arg ) STOP 52

  IntFun = REPEAT( CHAR(Arg), Arg)

  END FUNCTION

  END

