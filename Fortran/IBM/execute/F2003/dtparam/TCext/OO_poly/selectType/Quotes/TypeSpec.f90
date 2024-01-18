! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/selectType/Quotes/TypeSpec.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  TypeSpec.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : TypeSpec
!*
!*  DATE                       : Jan. 27, 2005
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
!*  the type spec
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE, ABSTRACT :: DT0(N1,K1)    ! (20,8)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: IArr(2)
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0) :: DT    ! (20,8)
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(*,8)), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM TypeSpec
  USE M
  IMPLICIT NONE

  TYPE(DT(20,8)), TARGET   ::  DTV(3,3,3)
  CLASS(*), POINTER :: Ptr(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J

    ALLOCATE(Ptr(2,2,2), SOURCE=1_8)

    SELECT TYPE (U => Ptr)
    CLASS DEFAULT

    SELECT TYPE (U => U(S,S,:))
      TYPE IS (INTEGER(KIND(DTV(1,1,1)%IArr)))
        PRINT*, "OK"
        IF (ANY(U .NE. 1_8)) STOP 22
      CLASS IS (DT0(*,8))
        STOP 23
      CLASS IS (DT(*,8))
        STOP 24

    END SELECT
    END SELECT


  END


