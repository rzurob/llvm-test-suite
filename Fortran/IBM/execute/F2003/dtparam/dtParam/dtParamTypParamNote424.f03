!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type parameters
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
!*  The code is copied from the note 4.24 on P.49
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypParamNote424
  IMPLICIT NONE


  !-- NOTE 4.24
  !The following example uses derived-type parameters.
  TYPE humongous_matrix(k, d)
    INTEGER, KIND :: k = kind(0.0)
    INTEGER(selected_int_kind(12)), LEN :: d
    !-- Specify a nondefault kind for d.
    REAL(k) :: element(d,d)
  END TYPE

  !In the following example, dim is declared to be a kind parameter, allowing generic overloading of
  !procedures distinguished only by dim.
  TYPE general_point(dim)
    INTEGER, KIND :: dim
    REAL :: coordinates(dim)
  END TYPE


  TYPE(humongous_matrix(d=3333)) :: T1
  TYPE(general_point(3)) :: T2

  IF ( T1%K       .NE. kind(0.0)   )                ERROR STOP 11
  IF ( KIND(T1%d) .NE. selected_int_kind(12)   )    ERROR STOP 12
  IF ( ANY( SHAPE(T1%element) .NE. (/T1%d, T1%d/))) ERROR STOP 13

  IF ( ANY(SHAPE(T2%coordinates) .NE. (/T2%dim/)))  ERROR STOP 14


  END


