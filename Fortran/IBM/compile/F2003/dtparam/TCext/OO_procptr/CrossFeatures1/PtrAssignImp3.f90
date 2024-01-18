! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/PtrAssignImp3.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:  tcomp PtrAssignImp3.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignImp3.f
!*
!*  DATE                       : Mar. 27, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  If proc-pointer-object has an implicit interface and is referenced
!*  as a subroutine, proc-target shall be a subroutine.
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Child(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id = 2
    END TYPE

  END MODULE

  SUBROUTINE ExtSub(Arg)
  USE M
  TYPE (Child(*,4)) :: Arg
    Arg = Child(20,4)(-2)
  END SUBROUTINE

  FUNCTION ExtFun()
  USE M
  TYPE (Child(20,4)) :: ExtFun
    ExtFun = Child(20,4)(-1)
  END FUNCTION

  PROGRAM PtrAssignImp3
  USE M
  IMPLICIT TYPE(Child(20,4))(C)

  INTERFACE
    SUBROUTINE Extsub(Arg)
      IMPORT Child
      TYPE (Child(*,4)) :: arg
    END SUBROUTINE

    FUNCTION ExtFun()
      IMPORT Child
      TYPE (Child(20,4)) :: ExtFun
    END FUNCTION
  END INTERFACE

  PROCEDURE(),  POINTER :: ProcPtr
  PROCEDURE(),  POINTER :: CProcPtr
  TYPE(Child(20,4))           :: V

  ProcPtr => ExtFun
  CProcPtr => ExtSub

  IF (.TRUE. ) THEN
    PRINT*, CProcPtr(V)
    CALL ProcPtr(Child(20,4)(-2))
  END IF

  END

