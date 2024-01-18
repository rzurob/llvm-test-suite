! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Misc/Misc8.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 16, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  Associating entity is poly by class is
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc8

    TYPE :: Base(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE
    TYPE, EXTENDS(Base) :: Child    ! (4)
    END TYPE

    SELECT TYPE ( As => Fun() )
      CLASS IS (Child(4))
        SELECT TYPE ( As )
          CLASS DEFAULT
        END SELECT
  END SELECT

  CONTAINS

  FUNCTION Fun()
    CLASS(Base(4)), ALLOCATABLE :: Fun
    ALLOCATE(Child(4) :: Fun)
  END FUNCTION

  END

