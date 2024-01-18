! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_procptr/CrossFeatures2/Assign1.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  Assign1.f 
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
!*  TEST CASE NAME             : Assign1.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 23, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
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
!*  A derived-type intrinsic assignment 
!*  (ICE)  
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc1 => ModFun1
    END TYPE

    TYPE, EXTENDS(BAse) :: DT    ! (4,20)
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc2 => ModFun2
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE PToP 
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION ModFun1(Arg)
    CLASS(Base(4,*)) :: Arg
    CLASS(Base(4,:)), POINTER :: ModFun1
      ALLOCATE(ModFun1, SOURCE=Arg)
    END FUNCTION

    FUNCTION ModFun2(Arg)
    CLASS(DT(4,*)) :: Arg
    CLASS(DT(4,:)), POINTER :: ModFun2
      ALLOCATE(ModFun2, SOURCE=Arg)
    END FUNCTION

    SUBROUTINE PToP (Arg1, Arg2)
    TYPE(DT(4,*)), INTENT (OUT) :: Arg1
    TYPE(DT(4,*)), INTENT (IN)  :: Arg2
      Arg1%Base = Arg2%Base  
      Arg1%ProcPtr2 => Arg2%ProcPtr2  
    END SUBROUTINE
 
  END MODULE


  PROGRAM Assign1 
  USE M
  IMPLICIT NONE 

  TYPE (DT(4,20))              :: V
  TYPE (Base(4,20))            :: V1
  TYPE (DT(4,:)), ALLOCATABLE :: V2
  TYPE (Base(4,:)), POINTER   :: V3

  PROCEDURE(ModFun1), POINTER :: ProcPtr1
  PROCEDURE(ModFun2), POINTER :: ProcPtr2

  ProcPtr1 => ModFun1 
  ProcPtr2 => ModFun2
 
  V = DT(4,20)(Base=Base(4,20)(ProcPtr1), ProcPtr2=ModFun2 )
  IF (.NOT. Equal(V, DT(4,20)(Base=Base(4,20)(ProcPtr1), ProcPtr2=ModFun2 )) ) STOP 11

  V1 = Base(4,20)(ModFun1)
  IF (.NOT. Equal(V1, Base(4,20)(ProcPtr1)) ) STOP 12

  ALLOCATE(V2, SOURCE=DT(4,20)(Base=Base(4,20)(ProcPtr1), ProcPtr2=ModFun2 ))
  IF (.NOT. Equal(V2, DT(4,20)(Base=Base(4,20)(ProcPtr1), ProcPtr2=ModFun2 )) ) STOP 13

  ALLOCATE(V3, SOURCE=Base(4,20)(ProcPtr1) )
  IF (.NOT. Equal(V3, Base(4,20)(ProcPtr1)) ) STOP 14


  CONTAINS

  FUNCTION Equal(Arg1, Arg2)
  LOGICAL Equal
  CLASS(Base(4,*)) :: Arg1, Arg2
  
  Equal = .FALSE.

  SELECT TYPE ( Arg1 )
  TYPE IS (Base(4,*))
    SELECT TYPE ( Arg2 )
    TYPE IS (Base(4,*) )
      Equal = ASSOCIATED(Arg1%ProcPtr1, Arg2%ProcPtr1) 
    CLASS DEFAULT
      STOP 22
    END SELECT
  TYPE IS (DT(4,*))  
    SELECT TYPE ( Arg2 )
    TYPE IS (DT(4,*) )
      Equal = ASSOCIATED(Arg1%ProcPtr1, Arg2%ProcPtr1) 
      Equal = Equal .AND. ASSOCIATED(Arg1%ProcPtr2, Arg2%ProcPtr2) 
    CLASS DEFAULT
      STOP 33
    END SELECT
  CLASS DEFAULT
    STOP 44
  END SELECT

  END FUNCTION

  END

