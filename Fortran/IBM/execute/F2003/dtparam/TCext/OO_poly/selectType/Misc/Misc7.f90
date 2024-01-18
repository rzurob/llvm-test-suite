! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/selectType/Misc/Misc7.f
! opt variations: -qnok -qnol -qnodeferredlp

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
!*  Associating entity is another associating entity
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc7

    TYPE :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE
    TYPE, EXTENDS(Base) :: Child    ! (4,20)
    END TYPE



    SELECT TYPE ( As => Fun() )
      CLASS DEFAULT
        SELECT TYPE ( As )
          CLASS DEFAULT
        END SELECT
  END SELECT

  CONTAINS

  FUNCTION Fun()
    CLASS(Base(4,:)), ALLOCATABLE :: Fun
    ALLOCATE(Child(4,20) :: Fun)
  END FUNCTION

  END

