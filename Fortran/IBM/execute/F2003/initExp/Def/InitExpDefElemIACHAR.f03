!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 07, 2006
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
!*  -  IACHAR
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemIACHAR
  IMPLICIT NONE
  INTEGER :: I, j

  CHARACTER, PARAMETER ::  C(128) = (/(ACHAR(I), I=0,127)/)
  INTEGER,   PARAMETER ::  K(128) = (/(I, I=0,127)/)

  TYPE :: DT
    INTEGER :: I(128)
  END TYPE

  INTEGER    :: I1(128)=IACHAR(C)
  TYPE(DT)   :: T1=DT(IACHAR(C))

  INTEGER(KIND=KIND(IACHAR(KIND=1, C=C)))  :: I2(128)=IACHAR(KIND=1, C=C)
  TYPE(DT)   :: T2=DT(IACHAR(KIND=1, C=C))

  INTEGER(KIND=KIND(IACHAR(KIND=1, C=(/(ACHAR(I),ACHAR(I+1), I=0,127,2)/))))  ::    &
            I3(128)=IACHAR(KIND=1, C=(/(ACHAR(I),ACHAR(I+1), I=0,127,2)/))
  TYPE(DT)   ::  T3=DT(IACHAR(KIND=1, C=(/(ACHAR(I),ACHAR(I+1), I=0,127,2)/)))


  IF ( ANY( I1     .NE. K  ) )      ERROR STOP 11
  IF ( ANY( T1%I   .NE. K  ) )      ERROR STOP 12

  IF ( KIND(I2)    .NE. 1 )         ERROR STOP 11
  IF ( ANY( I2     .NE. K  ) )      ERROR STOP 12
  IF ( ANY( T2%I   .NE. K  ) )      ERROR STOP 13

  IF ( KIND(I3)    .NE. 1 )         ERROR STOP 31
  IF ( ANY( I3     .NE. K  ) )      ERROR STOP 32
  IF ( ANY( T3%I   .NE. K  ) )      ERROR STOP 33



  END

