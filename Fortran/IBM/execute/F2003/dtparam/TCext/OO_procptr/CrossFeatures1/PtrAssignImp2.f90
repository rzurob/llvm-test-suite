! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=none /tstdev/OO_procptr/CrossFeatures1/PtrAssignImp2.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: PtrAssignImp2.f 
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
!*  TEST CASE NAME             : PtrAssignImp2.f 
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
!*  If proc-pointer-object has an implicit interface and is referenced 
!*  as a subroutine, proc-target shall be a subroutine. 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id = 1
    END TYPE

    TYPE :: Child(K2,N2)    ! (4,20)
      INTEGER, KIND     :: K2
      INTEGER, LEN      :: N2
      TYPE(Base(N2,K2)) :: BaseComp
    END TYPE

  END MODULE

  SUBROUTINE ExtSub(Arg)
  USE M
  TYPE (Child(4,*)) :: Arg 
    Arg = Child(4,20)(Base(20,4)(-1))
  END SUBROUTINE 

  PROGRAM PtrAssignImp2
  USE M 
  IMPLICIT TYPE(Child(4,20))(C) 

  INTERFACE
    SUBROUTINE Extsub(Arg)
     IMPORT Child
     TYPE (Child(4,*)) :: arg 
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(ExtSub),  POINTER :: ProcPtr
  PROCEDURE(ExtSub),  POINTER :: CProcPtr
  TYPE(Child(4,20))           :: V,W

  ProcPtr => ExtSub
  CALL  ProcPtr(V)
  IF ( V%BaseComp%ID   .NE. -1 ) STOP 11

  CProcPtr => ExtSub
  CALL  CProcPtr(W)
  IF ( W%BaseComp%ID   .NE. -1 ) STOP 21

  END 

