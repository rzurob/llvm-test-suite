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
!*  -  DIM
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemDIM
  IMPLICIT NONE
  INTEGER :: I, J, K
  REAL    :: r

  TYPE :: DT0
    INTEGER(1)   :: XI1(4,4) = -0
    INTEGER(2)   :: XI2      = -1
    INTEGER(4)   :: XI4(4,4) =  0
    INTEGER(8)   :: XI8      =  1

    INTEGER(1)   :: XII1(4,4) = 1
    INTEGER(2)   :: XII2      = 0
    INTEGER(4)   :: XII4(4,4) = -0
    INTEGER(8)   :: XII8      = -1

    REAL(4)      :: XR4(4,4) = -2.0
    REAL(8)      :: XR8      = -0.0
    REAL(16)     :: XR6(4,4) = +1.0

    REAL(4)      :: XRR4(4,4) = +1.0
    REAL(8)      :: XRR8      = -2.0
    REAL(16)     :: XRR6(4,4) = -1.0
  END TYPE

  TYPE(DT0), PARAMETER :: P=DT0()

  TYPE :: DT
    INTEGER(KIND(DIM(P%XI1, P%XII1)))   :: TI1(4,4) = DIM(P%XI1, P%XII1)
    INTEGER(KIND(DIM(P%XI2, P%XII2)))   :: TI2(4,4) = DIM(P%XI2, P%XII2)
    INTEGER(KIND(DIM(P%XI4, P%XII4)))   :: TI4(4,4) = DIM(P%XI4, P%XII4)
    INTEGER(KIND(DIM(P%XI8, P%XII8)))   :: TI8(4,4) = DIM(P%XI8, P%XII8)

    REAL(KIND(DIM(P%XR4, P%XRR4)))       :: TR4(4,4) = DIM(P%XR4, P%XRR4)
    REAL(KIND(DIM(P%XR8, P%XRR8)))       :: TR8(4,4) = DIM(P%XR8, P%XRR8)
    REAL(KIND(DIM(P%XR6, P%XRR6)))       :: TR6(4,4) = DIM(P%XR6, P%XRR6)
  END TYPE

  TYPE(DT) :: T


  IF ( KIND(T%TI1) .NE. 1 )   ERROR STOP 11
  IF ( ANY(T%TI1   .NE. 0  )) ERROR STOP 12
  IF ( KIND(T%TI2) .NE. 2 )   ERROR STOP 13
  IF ( ANY(T%TI2   .NE. 0  )) ERROR STOP 14
  IF ( KIND(T%TI4) .NE. 4 )   ERROR STOP 15
  IF ( ANY(T%TI4   .NE. 0  )) ERROR STOP 16
  IF ( KIND(T%TI8) .NE. 8 )   ERROR STOP 17
  IF ( ANY(T%TI8   .NE. 2  )) ERROR STOP 18

  IF ( KIND(T%TR4) .NE. 4 )   ERROR STOP 21
  IF ( ANY(T%TR4   .NE. 0 ))  ERROR STOP 22
  IF ( KIND(T%TR8) .NE. 8 )   ERROR STOP 23
  IF ( ANY(T%TR8   .NE. 2 ))  ERROR STOP 24
  IF ( KIND(T%TR6) .NE. 16)   ERROR STOP 25
  IF ( ANY(T%TR6   .NE. 2 ))  ERROR STOP 26

  END


