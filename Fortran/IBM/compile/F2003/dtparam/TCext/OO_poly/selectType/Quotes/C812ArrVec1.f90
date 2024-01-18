! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/C812ArrVec1.f
! opt variations: -qnok -qnol -qnodeferredlp

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
! %POSTCMD: tcomp C812ArrVec1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C812ArrVec1
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C812
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
!*    The selector is an array section  with a vector subscript
!*    A select type construct is embeded in a class default block
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE, abstract :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
    END TYPE

  END MODULE

  PROGRAM C812ArrVec1
  USE M
  IMPLICIT NONE

  CLASS(Base(4,:)), ALLOCATABLE :: Ptr(:,:)

  ALLOCATE( Child(4,20) :: Ptr(2:10, 3:12) )

  SELECT TYPE ( As => Ptr((/10,7,7,2/), (/12,3,3,12/)) )
    CLASS IS (Child(4,*))
      STOP 10
    TYPE IS (Child(4,*))
    CLASS DEFAULT
      SELECT TYPE ( As => As )
        TYPE IS (Child(4,*))
          As = Child(4,20)()
      END SELECT
  END SELECT
  STOP 40

  END

