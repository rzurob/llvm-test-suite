! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/F2003/initExp/Def/InitExpDefElemEXP.f
! opt variations: -qnok -qnol -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemExp.f
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
!*  -  Exp
!*  (318967)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemExp
  IMPLICIT NONE
  INTEGER :: I, J, K


  TYPE :: DT0(N1,K1,K2,K3,K4,K5,K6)    ! (20,4,8,16,4,8,16)
    INTEGER, KIND :: K1,K2,K3,K4,K5,K6
    INTEGER, LEN  :: N1
    REAL(K1)      :: XR4(4,4) =  1.0
    REAL(K2)      :: XR8      =  1.0
    REAL(K3)      :: XR6      =  1.0
    COMPLEX(K4)   :: XZ4(4,4) =  (1., 1.)
    COMPLEX(K5)   :: XZ8      =  (1., 1.)
    COMPLEX(K6)   :: XZ6(4,4) =  (1., 1.)
  END TYPE

  REAL, PARAMETER :: R=EXP(1.)
  COMPLEX, PARAMETER :: Z=EXP((1., 1.))

  TYPE(DT0(20,4,8,16,4,8,16)), PARAMETER :: P=DT0(20,4,8,16,4,8,16)()

  TYPE :: DT(K7,N2)    ! (4,20)
    INTEGER, KIND :: K7
    INTEGER, LEN  :: N2
    REAL(KIND(EXP(P%XR4)))       :: TR4(4,4) = EXP(P%XR4)
    REAL(KIND(EXP(P%XR8)))       :: TR8(4,4) = EXP(P%XR8)
    REAL(KIND(EXP(P%XR6)))       :: TR6(4,4) = EXP(P%XR6)

    COMPLEX(KIND(EXP(P%XZ4)))    :: TZ4(4,4) = RESHAPE((/(EXP(P%XZ4), I=1,16)/), (/4,4/))
    COMPLEX(KIND(EXP(P%XZ8)))    :: TZ8(4,4) = RESHAPE((/(EXP(P%XZ8), I=1,16)/), (/4,4/))
    COMPLEX(KIND(EXP(P%XZ6)))    :: TZ6(4,4) = RESHAPE((/(EXP(P%XZ6), I=1,16)/), (/4,4/))
  END TYPE

  TYPE(DT(4,20)) :: T

  IF ( KIND(T%TR4) .NE. 4 )    STOP 11
  IF ( KIND(T%TR8) .NE. 8 )    STOP 12
  IF ( KIND(T%TR6) .NE. 16 )   STOP 13

  IF ( KIND(T%TZ4) .NE. 4 )    STOP 21
  IF ( KIND(T%TZ8) .NE. 8 )    STOP 22
  IF ( KIND(T%TZ6) .NE. 16 )   STOP 23

  IF ( ANY( ABS(T%TR4-R) .GE. 1.E-5 ) )  STOP 31
  IF ( ANY( ABS(T%TR8-R) .GE. 1.E-5 ) )  STOP 32
  IF ( ANY( ABS(T%TR6-R) .GE. 1.E-5 ) )  STOP 33

  IF ( ANY( ABS(T%TZ4-Z) .GE. 1.E-5 ) )  STOP 41
  IF ( ANY( ABS(T%TZ8-Z) .GE. 1.E-5 ) )  STOP 42
  IF ( ANY( ABS(T%TZ6-Z) .GE. 1.E-5 ) )  STOP 43


  END


