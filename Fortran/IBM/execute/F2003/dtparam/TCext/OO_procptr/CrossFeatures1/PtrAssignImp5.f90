! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/PtrAssignImp5.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: PtrAssignImp5.f 
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
!*  TEST CASE NAME             : PtrAssignImp5.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 27, 2005
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
!*  If proc-target and proc-pointer-object are functions, 
!*  they shall have the same type; corresponding type parameters
!*  shall either both be deferred or both have the same value.
!* 
!*  (Mem Fault) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id = 1
    END TYPE

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(4)), POINTER :: ExtFun(:) 
  TYPE(Base(4))          :: Arg(:) 
    !ALLOCATE(ExtFun(2:1+SIZE(Arg)), SOURCE=Arg)
    ALLOCATE(ExtFun(2:1+SIZE(Arg)))
    ExtFun = Arg
  END FUNCTION

  PROGRAM PtrAssignImp5
  USE M
  IMPLICIT NONE 

  INTERFACE
    FUNCTION ExtFun(Arg)
      IMPORT
      TYPE(Base(4)), POINTER :: ExtFun(:) 
      TYPE(Base(4))          :: Arg(:) 
    END FUNCTION
  END INTERFACE

  PROCEDURE(ExtFun),   POINTER :: ProcPtr
  TYPE (Base(4)) :: V(3) = Base(4)(0)

  ProcPtr => ExtFun
  V = ProcPtr((/Base(4)(-1), Base(4)(-1), Base(4)(-1)/) )

  IF ( ANY(V%ID   .NE. -1 )) STOP 21

  END

