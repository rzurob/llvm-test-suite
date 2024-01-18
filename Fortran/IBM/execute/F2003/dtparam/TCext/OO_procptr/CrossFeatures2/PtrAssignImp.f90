! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/OO_procptr/CrossFeatures2/PtrAssignImp.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: PtrAssignImp.f 
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
!*  TEST CASE NAME             : PtrAssignImp.f 
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
!*  explicit  interface
!*  (ice-314926) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: BaseId = 1
      PROCEDURE(GetBaseId), PASS, POINTER :: ProcPtr1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child(N2,K2)    ! (20,4,20,4)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      INTEGER(K2)   :: ChildId = 2
      PROCEDURE(GetChildId), PASS, POINTER :: ProcPtr2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child(*,4,*,4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base(*,4)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION
 
  END MODULE

  FUNCTION ExtFun()
  USE M
  TYPE (Child(20,4,20,4)) :: ExtFun
    ExtFun = Child(20,4,20,4)(BaseId=-1, ChildID=-2, ProcPtr1=GetBaseId, ProcPtr2=GetChildId)
  END FUNCTION

  PROGRAM PtrAssignImp
  USE M 
  IMPLICIT NONE 

  INTERFACE
    FUNCTION ExtFun()
     IMPORT Child
     TYPE (Child(20,4,20,4)) :: ExtFun
    END FUNCTION
  END INTERFACE

  CLASS(Child(:,4,:,4)), ALLOCATABLE :: V

  ALLOCATE(V, SOURCE=ExtFun())

  IF ( V%Base%GetID() .NE. V%Base%ProcPtr1() ) STOP 11
  IF ( V%GetID()     .NE. V%ProcPtr2()      ) STOP 12

  SELECT TYPE (V)
  TYPE IS (Child(*,4,*,4)) 
    V = V
  CLASS DEFAULT 
    STOP  44
  END SELECT

  IF ( V%Base%GetID() .NE. V%Base%ProcPtr1() ) STOP 21
  IF ( V%GetID()     .NE. V%ProcPtr2()      ) STOP 22

  END 

