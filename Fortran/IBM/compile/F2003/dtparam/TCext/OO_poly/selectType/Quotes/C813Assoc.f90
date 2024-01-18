! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/selectType/Quotes/C813Assoc.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp C813Assoc.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C813Assoc
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
!*    The selector is an associating entity of poly with an  extension type
!*    to thst of "TYPE IS"
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Ground(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Ground) :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
    END TYPE

  END MODULE

  PROGRAM C813Assoc
  USE M
  IMPLICIT NONE

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: ARg


  SELECT TYPE (Arg)
  TYPE IS (Child(4,*))
    ASSOCIATE ( As =>Arg )
    SELECT TYPE ( As )
      TYPE IS (Child(4,*))
      CLASS DEFAULT
    END SELECT
    END ASSOCIATE
  END SELECT

  END SUBROUTINE

  END

