! GB DTP extension using:
! ftcx_dtp -qreuse=self /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrTarget.f
! opt variations: -qck -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 08, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Target
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(N1,K1)    ! (1,4)
    INTEGER, KIND          :: K1
    INTEGER, LEN           :: N1
    CHARACTER(N1), PRIVATE :: C0="!"
    INTEGER(K1)            :: I=0
    CHARACTER(N1), PRIVATE :: C1="!"
  END TYPE

  END MODULE

  PROGRAM dataPtrTarget
  USE M
  IMPLICIT NONE

  TYPE(DT(1,4)),   TARGET  :: Arr(100, 100), Arr1(10000)
  INTEGER ,   POINTER :: Ptr(:, :)
  INTEGER             :: I, J


  DO I =1, 100
  DO J =I, 100

    Arr(I:, J:) = DT(1,4)(I=-I)

    Ptr(I:, J:) => Arr(I:, J:)%I
    IF (.NOT. ASSOCIATED(Ptr, Arr(I:, J:)%I ))  ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I , J/)))       ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/100,  100 /)))  ERROR STOP 13
    IF (ANY( Ptr         .NE. -I ))             ERROR STOP 14

    Arr1(1:(J-I+1)*(J-I+1)) = DT(1,4)(I=-I)
    Ptr(I:J, I:J) => Arr1%I
    IF (.NOT. ASSOCIATED(Ptr))                  ERROR STOP 21
    IF (SIZE(Ptr) .NE. (J-I+1)*(J-I+1))         ERROR STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I , I /)))      ERROR STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J , J /)))      ERROR STOP 23
    IF (ANY( Ptr         .NE. -I ))             ERROR STOP 24

  END DO
  END DO

  END

