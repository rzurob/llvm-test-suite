! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Quotes/C813ArrZero.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C813
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
!*    The selector is an associating entity of poly array of zero size
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Ground(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Ground) :: Base    ! (4)
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
    END TYPE

  END MODULE

  PROGRAM C813ArrZero
  USE M
  IMPLICIT NONE
  CLASS(*), ALLOCATABLE :: Arr(:)

  ALLOCATE( Base(4) :: Arr(4) )

  SELECT TYPE ( As => Arr(1:0) )
  TYPE IS (Base(4))
    SELECT TYPE ( As )
      TYPE IS (Child(4))
        STOP 51
      CLASS DEFAULT
        STOP 31
    END SELECT
  END SELECT

  END

