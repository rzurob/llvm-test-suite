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
!*  -  Exp
!*  (318967)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemExp
  IMPLICIT NONE
  INTEGER :: I, J, K


  TYPE :: DT0
    REAL(4)      :: XR4(4,4) =  1.0
    REAL(8)      :: XR8      =  1.0
    REAL(16)     :: XR6      =  1.0
    COMPLEX(4)   :: XZ4(4,4) =  (1., 1.)
    COMPLEX(8)   :: XZ8      =  (1., 1.)
    COMPLEX(16)  :: XZ6(4,4) =  (1., 1.)
  END TYPE

  REAL, PARAMETER :: R=EXP(1.)
  COMPLEX, PARAMETER :: Z=EXP((1., 1.))

  TYPE(DT0), PARAMETER :: P=DT0()

  TYPE :: DT
    REAL(KIND(EXP(P%XR4)))       :: TR4(4,4) = EXP(P%XR4)
    REAL(KIND(EXP(P%XR8)))       :: TR8(4,4) = EXP(P%XR8)
    REAL(KIND(EXP(P%XR6)))       :: TR6(4,4) = EXP(P%XR6)

    COMPLEX(KIND(EXP(P%XZ4)))    :: TZ4(4,4) = RESHAPE((/(EXP(P%XZ4), I=1,16)/), (/4,4/))
    COMPLEX(KIND(EXP(P%XZ8)))    :: TZ8(4,4) = RESHAPE((/(EXP(P%XZ8), I=1,16)/), (/4,4/))
    COMPLEX(KIND(EXP(P%XZ6)))    :: TZ6(4,4) = RESHAPE((/(EXP(P%XZ6), I=1,16)/), (/4,4/))
  END TYPE

  TYPE(DT) :: T

  IF ( KIND(T%TR4) .NE. 4 )    ERROR STOP 11
  IF ( KIND(T%TR8) .NE. 8 )    ERROR STOP 12
  IF ( KIND(T%TR6) .NE. 16 )   ERROR STOP 13

  IF ( KIND(T%TZ4) .NE. 4 )    ERROR STOP 21
  IF ( KIND(T%TZ8) .NE. 8 )    ERROR STOP 22
  IF ( KIND(T%TZ6) .NE. 16 )   ERROR STOP 23

  IF ( ANY( ABS(T%TR4-R) .GE. 1.E-5 ) )  ERROR STOP 31
  IF ( ANY( ABS(T%TR8-R) .GE. 1.E-5 ) )  ERROR STOP 32
  IF ( ANY( ABS(T%TR6-R) .GE. 1.E-5 ) )  ERROR STOP 33

  IF ( ANY( ABS(T%TZ4-Z) .GE. 1.E-5 ) )  ERROR STOP 41
  IF ( ANY( ABS(T%TZ8-Z) .GE. 1.E-5 ) )  ERROR STOP 42
  IF ( ANY( ABS(T%TZ6-Z) .GE. 1.E-5 ) )  ERROR STOP 43


  END


