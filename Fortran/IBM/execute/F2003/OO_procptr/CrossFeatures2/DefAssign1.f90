! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: DefAssign1.f
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
!*  TEST CASE NAME             : DefAssign1.f 
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
!*  Defined assignment - elemental
!*  (ICE on str constr) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


    TYPE :: Base
      iNTEGER :: Id=0
    END TYPE

    TYPE, EXTENDS(Base) :: DT
      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc => Modfun
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT) :: Arg
    TYPE(DT) :: ModFun
      ModFun = Arg
    END FUNCTION

    SUBROUTINE MyAssign1 (Arg1, Arg2)
    TYPE(Base), INTENT(OUT) :: Arg1(:) 
    TYPE(Base), INTENT(IN)  :: Arg2 
      Arg1 = Arg2
    END SUBROUTINE 
 
    SUBROUTINE MyAssign2 (Arg1, arg2)
    TYPE(DT),    INTENT(OUT) :: Arg1(:) 
    TYPE(Base),  INTENT(IN)  :: Arg2 
      Arg1%Id = Arg2%Id
      DO i=1, SIZE(Arg1) 
        Arg1(i)%ProcPtr => ModFun 
      END DO
    END SUBROUTINE 
 
    SUBROUTINE MyAssign3 (Arg1, Arg2)
    TYPE(Base), INTENT(OUT) :: Arg1(:) 
    TYPE(DT),   INTENT(IN)  :: Arg2 
      Arg1 = Arg2%Base
    END SUBROUTINE 
 
    SUBROUTINE MyAssign4 (Arg1, Arg2)
    TYPE(DT),  INTENT(OUT) :: Arg1(:) 
    TYPE(DT),  INTENT(IN)  :: Arg2 
      Arg1%Id = Arg2%Id 
      DO i=1, SIZE(Arg1) 
        Arg1(i)%ProcPtr =>  Arg2%ProcPtr 
      END DO
    END SUBROUTINE 
 
 
  END MODULE


  PROGRAM DefAssign1 
  USE M
  IMPLICIT NONE 

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE MyAssign1
      MODULE PROCEDURE MyAssign2
      MODULE PROCEDURE MyAssign3
      MODULE PROCEDURE MyAssign4
    END INTERFACE ASSIGNMENT ( = ) 

  INTEGER :: I
  TYPE(Base) :: B1(511), B2(511)
  TYPE(DT)   :: D1(511), D2(511)
 
  B1 = Base(1)
  DO I=1, 511
    IF (B1(I)%Id .NE. 1) STOP 11
  END DO

  D1 = Base(2)
  DO I=1, 511
    IF (.NOT. ASSOCIATED(D1(I)%ProcPtr, ModFun)) STOP 22
    IF (D1(I)%Id .NE. 2) STOP 23   
  END DO

  B2 = DT(-1, ModFun) 
  DO I=1, 511
    IF (B2(I)%Id .NE. -1) STOP 31   
  END DO

  D2 = DT(0, NULL())
  D2 = DT(-2, ModFun)
  DO I=1, 511
    IF (.NOT. ASSOCIATED(D2(I)%ProcPtr, ModFun)) STOP 62
    IF (D2(I)%Id .NE. -2) STOP 63   
  END DO


  END

