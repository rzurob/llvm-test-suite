! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/OO_procptr/CrossFeatures2/PtrAssignImp2.f
! opt variations: -qnol -qdeferredlp -qreuse=none

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
!*  (ice/315447) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: BaseId = 1
      PROCEDURE(IExt), NOPASS, POINTER :: ProcPtr0
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (20,4)
      INTEGER(K1)  :: ChildId = 2
      PROCEDURE(IExt), NOPASS, POINTER :: ProcPtr1
    END TYPE

!   INTERFACE
!     SUBROUTINE IExt(Arg)
!      IMPORT Base 
!      CLASS(Base) :: arg 
!   END SUBROUTINE
!   END INTERFACE

  CONTAINS
      SUBROUTINE IExt(Arg)
       CLASS(Base(*,4)) :: arg
    END SUBROUTINE

  END MODULE

  SUBROUTINE ExtSub(Arg)
  USE M
  CLASS (Base(*,4)) :: Arg
    SELECT TYPE (Arg)
    TYPE IS (Base(*,4))
      Arg = Base(20,4)(BaseID =-1, ProcPtr0=IExt)
    TYPE IS (Child(*,4)) 
      Arg = Child(20,4)(BaseId=-1, ChildID=-2,  ProcPtr0=IExt,  ProcPtr1=IExt)
    END SELECT
  END SUBROUTINE 

  PROGRAM PtrAssignImp2
  USE M 
  IMPLICIT TYPE(Base(20,4))(C) 

  PROCEDURE(IExt)         :: ExtSub
  PROCEDURE(IExt),POINTER :: ProcPtr
  PROCEDURE(),  POINTER   :: CProcPtr
  TYPE(Base(20,4) )               :: V1
  TYPE(Child(20,4))               :: V2
  CLASS(Base(20,4)),  ALLOCATABLE :: V3
  CLASS(Child(20,4)), ALLOCATABLE :: V4

  ProcPtr => ExtSub

  CALL  ProcPtr(V1)
  IF ( V1%BaseID      .NE. -1 ) STOP 11
  IF ( .NOT. ASSOCIATED(V1%ProcPtr0, IExt) ) STOP 12
 
  CALL  ProcPtr(V2)
  IF ( V2%BaseID      .NE. -1 ) STOP 21
  IF ( V2%Base%BaseID .NE. -1 ) STOP 22
  IF ( V2%ChildID     .NE. -2 ) STOP 23
  IF ( .NOT. ASSOCIATED(V2%ProcPtr0, IExt) ) STOP 24
  IF ( .NOT. ASSOCIATED(V2%ProcPtr1, IExt) ) STOP 25

  ALLOCATE(V3)
  CALL  ProcPtr(V3)
  IF ( V3%BaseID      .NE. -1 ) STOP 31
  IF ( .NOT. ASSOCIATED(V3%ProcPtr0, IExt) ) STOP 32 

  ALLOCATE(V4)
  CALL  ProcPtr(V4)
  IF ( V4%BaseID      .NE. -1 ) STOP 41
  IF ( V4%Base%BaseID .NE. -1 ) STOP 42
  IF ( V4%ChildID     .NE. -2 ) STOP 43
  IF ( .NOT. ASSOCIATED(V4%ProcPtr0, IExt) ) STOP 44 
  IF ( .NOT. ASSOCIATED(V4%ProcPtr1, IExt) ) STOP 45 


  END 

