! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_poly/selectType/CrossFeatures/SelectType1.f
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
!* Select Type
!* (299308)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SelectType1
  IMPLICIT CLASS(*)(U)
  TYPE :: DT(K1,K2,N1)    ! (4,1,3)
    INTEGER, KIND             :: K1,K2
    INTEGER, LEN              :: N1
    INTEGER(K1)               :: Int
    CHARACTER(kind=K2,len=N1) :: C
  END TYPE
  INTEGER :: i

  CALL Sub(DT(4,1,3)(Int=6, C="!"))

  CONTAINS

  SUBROUTINE Sub(U)

A:SELECT TYPE (U)
  CLASS IS (DT(4,1,*))

    ASSOCIATE (U => U)
      associate:SELECT TYPE(U)
      CLASS DEFAULT
        select: ASSOCIATE ( U => U)
          IF ( U%Int .NE. 6 )       ERROR STOP 30
          IF ( TRIM(U%C) .NE. "!" ) ERROR STOP 30
        END ASSOCIATE select
      END SELECT associate
    END ASSOCIATE

  CLASS DEFAULT
    STOP 40
  END SELECT A

  END SUBROUTINE

  END


