! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/initExp/Def/InitExpDefElemDBLE.f
! opt variations: -qnok -ql -qreuse=self

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
!*  -  DBLE
!*  (318967)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemDBLE
  IMPLICIT NONE
  INTEGER :: I, J, K
  REAL    :: r

  TYPE :: DT0(K1,K2,K3,K4,K5,K6,K7,K8,K9,K10)    ! (1,2,4,8,4,8,16,4,8,16)
    INTEGER, KIND :: K1,K2,K3,K4,K5,K6,K7,K8,K9,K10
    INTEGER(K1)   :: XI1(4,4) = -1
    INTEGER(K2)   :: XI2(4,4) = -1
    INTEGER(K3)   :: XI4(4,4) = -1
    INTEGER(K4)   :: XI8(4,4) = -1

    REAL(K5)      :: XR4(4,4) = -1.0
    REAL(K6)      :: XR8(4,4) = -1.0
    REAL(K7)      :: XR6(4,4) = -1.0

    COMPLEX(K8)   :: XZ4(4,4) = (-1.0, 0.0)
    COMPLEX(K9)   :: XZ8(4,4) = (-1.0, 0.0)
    COMPLEX(K10)  :: XZ6(4,4) = (-1.0, 0.0)
  END TYPE

  TYPE(DT0(1,2,4,8,4,8,16,4,8,16)), PARAMETER :: P=DT0(1,2,4,8,4,8,16,4,8,16)()

  TYPE :: DT(K11)    ! (4)
    INTEGER, KIND :: K11
    REAL(KIND(DBLE(P%XI1)))   :: TI1(4,4) = DBLE(P%XI1)
    REAL(KIND(DBLE(P%XI2)))   :: TI2(4,4) = DBLE(P%XI1)
    REAL(KIND(DBLE(P%XI4)))   :: TI4(4,4) = DBLE(P%XI1)
    REAL(KIND(DBLE(P%XI8)))   :: TI8(4,4) = DBLE(P%XI1)

    REAL(KIND(DBLE(P%XR4)))   :: TR4(4,4) = DBLE(P%XR4)
    REAL(KIND(DBLE(P%XR8)))   :: TR8(4,4) = DBLE(P%XR8)
    REAL(KIND(DBLE(P%XR6)))   :: TR6(4,4) = DBLE(P%XR6)

    REAL(KIND(DBLE(P%XZ4)))   :: TZ4(4,4) = DBLE(P%XZ4)
    REAL(KIND(DBLE(P%XZ8)))   :: TZ8(4,4) = DBLE(P%XZ8)
    REAL(KIND(DBLE(P%XZ6)))   :: TZ6(4,4) = DBLE(P%XZ6)
  END TYPE

  TYPE(DT(4)) :: T


  IF ( KIND(T%TI1) .NE. 8 )   STOP 11
  IF ( ANY(T%TI1   .NE. -1 )) STOP 12
  IF ( KIND(T%TI2) .NE. 8 )   STOP 13
  IF ( ANY(T%TI2   .NE. -1 )) STOP 14
  IF ( KIND(T%TI4) .NE. 8 )   STOP 15
  IF ( ANY(T%TI4   .NE. -1 )) STOP 16
  IF ( KIND(T%TI8) .NE. 8 )   STOP 17
  IF ( ANY(T%TI8   .NE. -1 )) STOP 18

  IF ( KIND(T%TR4) .NE. 8 )   STOP 21
  IF ( ANY(T%TR4   .NE. -1 )) STOP 22
  IF ( KIND(T%TR8) .NE. 8 )   STOP 23
  IF ( ANY(T%TR8   .NE. -1 )) STOP 24
  IF ( KIND(T%TR6) .NE. 8 )   STOP 25
  IF ( ANY(T%TR6   .NE. -1 )) STOP 26

  IF ( KIND(T%TZ4) .NE. 8 )   STOP 31
  IF ( ANY(T%TZ4   .NE. -1 )) STOP 32
  IF ( KIND(T%TZ8) .NE. 8 )   STOP 33
  IF ( ANY(T%TZ8   .NE. -1 )) STOP 34
  IF ( KIND(T%TZ6) .NE. 8 )   STOP 35
  IF ( ANY(T%TZ6   .NE. -1 )) STOP 36

  END


