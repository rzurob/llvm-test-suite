! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_poly/selectType/CrossFeatures/SelectCase1.f
! opt variations: -qnock

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 04, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Select Case
!* (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SelectCase1
  IMPLICIT CLASS(*)(U)
  TYPE :: DT(K1,K2,N1)    ! (4,1,3)
    INTEGER, KIND             :: K1,K2
    INTEGER, LEN              :: N1
    INTEGER(K1)               :: Int
    CHARACTER(kind=K2,len=N1) :: C
  END TYPE
  INTEGER :: i

  CALL Sub(6_8)

  CONTAINS

  SUBROUTINE Sub(U)

A:SELECT TYPE (U)
  TYPE IS (INTEGER(4))
    STOP 30
  TYPE IS (INTEGER(8))

B:  SELECT CASE (U)
    CASE (:5_8)
      STOP 20
    CASE (6_8)
      PRINT *, "OK!"
    CASE (7_8:)
      STOP 20
    END SELECT B

  CLASS DEFAULT
    STOP 40
  END SELECT A

  END SUBROUTINE

  END



