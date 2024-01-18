!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemDPROD.f
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
!*  -  DPROD
!*  (318967)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemDPROD
  IMPLICIT NONE
  INTEGER :: I, J, K


  TYPE :: DT0
    REAL(4)      :: XR4(4,4) = -2.0
    REAL(4)      :: XRR4     =  2.0
  END TYPE

  TYPE(DT0), PARAMETER :: P=DT0()

  TYPE :: DT
    REAL(KIND(DPROD(P%XR4, P%XRR4)))       :: TR44(4,4) = DPROD(P%XR4, P%XRR4)
  END TYPE

  TYPE(DT) :: T

  TYPE(DT0), PARAMETER :: PP(4)=DT0()

  TYPE :: DT1
    REAL(KIND(DPROD(PP(1)%XR4, PP(2)%XRR4)))    &
        :: TR44(4) = DPROD( (/(PP(I)%XR4(I,I), I=1,4)/), (/(PP(I)%XRR4, I=1,4)/))
  END TYPE

  TYPE(DT1) :: TT


  IF ( KIND(T%TR44) .NE. 8 )    STOP 11
  IF ( ANY(T%TR44   .NE. -4 ))  STOP 12

  IF ( KIND(TT%TR44) .NE. 8 )   STOP 21
  IF ( ANY(TT%TR44   .NE. -4 )) STOP 22

  END


