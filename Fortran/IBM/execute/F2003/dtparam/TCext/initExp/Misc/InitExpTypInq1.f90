! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/initExp/Misc/InitExpTypInq1.f
! opt variations: -qck -qnol -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 22, 2006
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
!*  Type Parameter Inquiry on intrinsic types
!*
!* (324775/324382)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypInq1
  IMPLICIT NONE

  INTEGER(1), parameter :: I1c(1) = 1
  INTEGER(2), parameter :: I2c(2) = 2
  INTEGER(4), parameter :: I4c(4) = 4
  INTEGER(8), parameter :: I8c(8) = 8

  LOGICAL(1), parameter :: L1c(1) = .true.
  LOGICAL(2), parameter :: L2c(2) = .true.
  LOGICAL(4), parameter :: L4c(4) = .true.
  LOGICAL(8), parameter :: L8c(8) = .true.

  REAL(4), parameter  :: R4c(4)   = 4
  REAL(8), parameter  :: R8c(8)   = 8
  REAL(16),parameter  :: R6c(16)  = 16

  COMPLEX(4), parameter  :: Z4c(4)   = (4.0, 4.0)
  COMPLEX(8), parameter  :: Z8c(8)   = (8.0, 8.0)
  COMPLEX(16),parameter  :: Z6c(16)  = (16.0, 16.0)

  CHARACTER(128), parameter :: C(128) = ''

  TYPE :: DT(N1,K1,K2,K3,K4)    ! (20,1,2,4,8)
    INTEGER, KIND :: K1,K2,K3,K4
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: I1=I1c%KIND
    INTEGER(K2)   :: I2=I2c%KIND
    INTEGER(K3)   :: I4=I4c%KIND
    INTEGER(K4)   :: I8=I8c%KIND

    INTEGER(K1)   :: L1=L1c%KIND
    INTEGER(K2)   :: L2=L2c%KIND
    INTEGER(K3)   :: L4=L4c%KIND
    INTEGER(K4)   :: L8=L8c%KIND

    INTEGER(K3)   :: R4=R4c%KIND
    INTEGER(K4)   :: R8=R8c%KIND
    INTEGER(K1)   :: R6=R6c%KIND

    INTEGER(K3)   :: Z4=Z4c%KIND
    INTEGER(K4)   :: Z8=Z8c%KIND
    INTEGER(K1)   :: Z6=Z6c%KIND

    CHARACTER(C%LEN) :: CC=ACHAR(C%KIND)
  END TYPE

  TYPE(DT(20,1,2,4,8)), PARAMETER ::  T=DT(20,1,2,4,8)()


  IF (T%I1    .NE. 1  ) STOP 11
  IF (T%I2    .NE. 2  ) STOP 12
  IF (T%I4    .NE. 4  ) STOP 14
  IF (T%I8    .NE. 8  ) STOP 18

  IF (T%L1    .NE. 1  ) STOP 21
  IF (T%L2    .NE. 2  ) STOP 22
  IF (T%L4    .NE. 4  ) STOP 24
  IF (T%L8    .NE. 8  ) STOP 28

  IF (T%R4    .NE. 4  ) STOP 31
  IF (T%R8    .NE. 8  ) STOP 32
  IF (T%R6    .NE. 16 ) STOP 33

  IF (T%Z4    .NE. 4  ) STOP 41
  IF (T%Z8    .NE. 8  ) STOP 42
  IF (T%Z6    .NE. 16 ) STOP 43

  IF (T%CC    .NE. ACHAR(1) ) STOP 51
  IF (LEN(T%CC).NE. 128)      STOP 52


  END


