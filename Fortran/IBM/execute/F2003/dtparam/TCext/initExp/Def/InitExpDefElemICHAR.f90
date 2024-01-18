! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Def/InitExpDefElemICHAR.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemICHAR.f
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
!*  -  ICHAR
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemICHAR
  IMPLICIT REAL(ICHAR(CHAR(16)))(R)
  INTEGER :: I, j

  INTEGER,   PARAMETER :: R16=16

  CHARACTER, PARAMETER ::  C(128) = (/(ACHAR(I), I=0,127)/)
  INTEGER,   PARAMETER ::  K(128) = (/(I, I=0,127)/)

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: I(128)
  END TYPE

  INTEGER    :: I1(128)=ICHAR(C)
  TYPE(DT(20,4))   :: T1=DT(20,4)(ICHAR(C))

  INTEGER(KIND=KIND(ICHAR(KIND=1, C=C)))  :: I2(128)=ICHAR(KIND=1, C=C)
  TYPE(DT(20,4))   :: T2=DT(20,4)(ICHAR(KIND=1, C=C))

  INTEGER(KIND=KIND(ICHAR(KIND=1, C=(/(ACHAR(I),ACHAR(I+1), I=0,127,2)/))))  ::    &
            I3(128)=ICHAR(KIND=1, C=(/(ACHAR(I),ACHAR(I+1), I=0,127,2)/))
  TYPE(DT(20,4))   ::  T3=DT(20,4)(ICHAR(KIND=1, C=(/(ACHAR(I),ACHAR(I+1), I=0,127,2)/)))

  IF ( KIND(R)     .NE. 16  )       STOP 10

  IF ( ANY( I1     .NE. K  ) )      STOP 11
  IF ( ANY( T1%I   .NE. K  ) )      STOP 12

  IF ( KIND(I2)    .NE. 1 )         STOP 11
  IF ( ANY( I2     .NE. K  ) )      STOP 12
  IF ( ANY( T2%I   .NE. K  ) )      STOP 13

  IF ( KIND(I3)    .NE. 1 )         STOP 31
  IF ( ANY( I3     .NE. K  ) )      STOP 32
  IF ( ANY( T3%I   .NE. K  ) )      STOP 33



  END


