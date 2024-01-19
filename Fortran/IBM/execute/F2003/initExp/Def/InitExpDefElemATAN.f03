!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 23, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to an elemental intrinsic
!*
!*  -ATAN
!*  (318932)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefElemATAN
  IMPLICIT NONE
  INTEGER :: I, J, K
  REAL(8), PARAMETER    :: r(8)=1.5574077_8

  CALL IntSub(r)

  CONTAINS

  SUBROUTINE IntSub(Arg)
  REAL(8) :: Arg(8)
  REAL(KIND(Arg)) :: T(KIND(Arg))=(/(NINT(ATAN(r(I:I))/ATAN(r(I:I))),  &
                                         I=NINT(ATAN(r(1))), KIND(Arg))/)

  LOGICAL(KIND(Arg)) :: L(KIND(Arg)) = ATAN(r) .EQ. ATAN(r)
  LOGICAL(KIND(Arg)) :: L1(KIND(Arg)) = ATAN(r) .NE. ATAN(r)

  IF (KIND(T) .NE. 8 )          ERROR STOP 11
  IF (SIZE(T) .NE. 8 )          ERROR STOP 12
  IF (ANY(T   .GT. 1.0  ) )     ERROR STOP 13

  IF (KIND(L) .NE. 8 )          ERROR STOP 21
  IF (SIZE(L) .NE. 8 )          ERROR STOP 22
  IF (ANY(L   .NEQV. .TRUE. ))  ERROR STOP 23

  IF (KIND(L1) .NE. 8 )         ERROR STOP 31
  IF (SIZE(L1) .NE. 8 )         ERROR STOP 32
  IF (ANY(L1   .NEQV. .FALSE.)) ERROR STOP 33


  END SUBROUTINE

  END


