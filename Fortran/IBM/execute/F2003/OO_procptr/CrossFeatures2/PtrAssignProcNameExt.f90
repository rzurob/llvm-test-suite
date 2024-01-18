! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: PtrAssignProcNameExt.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameExt.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 12, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment 
!*
!*  REFERENCE                  : Feature 289058 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!* 
!* 
!*  (315269) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  INTERFACE

    FUNCTION IExtFun(Arg)
    CLASS(*)          :: Arg(:)
    CLASS(*), POINTER :: IExtFun(:)
    END FUNCTION

    SUBROUTINE IExtSub(Arg1, Arg2)
    IMPORT
    PROCEDURE(IExtFun) :: Arg1
    CLASS(*)           :: Arg2(:)
    END SUBROUTINE

  END INTERFACE

  END MODULE


  PROGRAM PtrAssignProcNameExt  
  USE M
  IMPLICIT NONE

  PROCEDURE(IExtFun),  POINTER :: Ptr
  PROCEDURE(IExtSub),  POINTER :: Ptr1
  PROCEDURE(IExtFun)           :: ExtFun
  PROCEDURE(IExtSub)           :: ExtSub

  Ptr => ExtFun
  SELECT TYPE ( As => Ptr((/3_8,2_8,1_8/)) )
  TYPE IS (INTEGER(8))
    IF ( ANY(LBOUND(As) .NE. (/1/)) ) STOP 11
    IF ( ANY(UBOUND(As) .NE. (/3/)) ) STOP 12
    IF ( ANY(As .NE. (/3_8,2_8,1_8/)) ) STOP 13
  CLASS DEFAULT
    STOP 14
  END SELECT

  Ptr1 => ExtSub
  CALL Ptr1(Ptr, (/.FALSE._2, .TRUE._2/))

  END

  FUNCTION ExtFun(Arg)
  CLASS(*)          :: Arg(:)
  CLASS(*), POINTER :: ExtFun(:) 
    ALLOCATE(ExtFun(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  PROCEDURE(IExtFun) :: Arg1
  CLASS(*)           :: Arg2(:)
    SELECT TYPE ( As => Arg1(Arg2) )
    TYPE IS (LOGICAL(2))
      IF ( ANY(LBOUND(As) .NE. (/1/))) STOP 21
      IF ( ANY(UBOUND(As) .NE. (/2/))) STOP 22
      IF ( ANY(As .NEQV. (/.FALSE._2, .TRUE._2/)) ) STOP 23
    CLASS DEFAULT
      STOP 24
    END SELECT
  END SUBROUTINE 



