!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 07, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  If a type-param-value in a char-length in an entity-decl is not a colon or
!*  an asterisk, it shall be a specification-expr.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC501


  IF ( SIZE( IntFun(10) ) .NE. 10 ) ERROR STOP 11
  IF ( LEN ( IntFun(10) ) .NE. 10 ) ERROR STOP 12
  IF ( ANY ( IntFun(10)   .NE. REPEAT( CHAR(10), 10) )) ERROR STOP 13

  CONTAINS
  FUNCTION IntFun(Arg)
  INTEGER          :: Arg
  CHARACTER        :: C*(Arg)
  CHARACTER(Arg-1) :: C1*(Arg)
  CHARACTER        :: C2(Arg)*(Arg)
  CHARACTER(Arg+1) :: C3(Arg)*(Arg)
  CHARACTER(Arg-1) :: IntFun(Arg)*(Arg)

  IF ( LEN ( C )  .NE. Arg  ) ERROR STOP 21

  IF ( LEN ( C1 ) .NE. Arg ) ERROR STOP 31

  IF ( LEN ( C2 ) .NE. Arg ) ERROR STOP 41
  IF ( SIZE( C2 ) .NE. Arg ) ERROR STOP 42

  IF ( LEN ( C3 ) .NE. Arg ) ERROR STOP 51
  IF ( SIZE( C3 ) .NE. Arg ) ERROR STOP 52

  IntFun = REPEAT( CHAR(Arg), Arg)

  END FUNCTION

  END
