! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Misc/Misc11.f
! opt variations: -qnok -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Misc11.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc11
!*
!*  DATE                       : Jan. 05, 2005
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
!*
!*  ALLOCATE: ICE - A type bound function return as SOURCE
!*    (297668)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    CONTAINS
      PROCEDURE, PASS   :: ReturnObj1
    END TYPE

    CONTAINS

    FUNCTION ReturnObj1(Arg)
    CLASS(Zero(4,*)) :: Arg
    CLASS(*), ALLOCATABLE :: ReturnObj1
      ALLOCATE(ReturnObj1, SOURCE=Arg)
    END FUNCTION

  END MODULE


  PROGRAM  Misc11
  USE M
  IMPLICIT NONE
  TYPE(Zero(4,20)) :: V
  CLASS(*), POINTER :: Ptr
  ALLOCATE(Ptr, SOURCE=V%ReturnObj1)

  END

