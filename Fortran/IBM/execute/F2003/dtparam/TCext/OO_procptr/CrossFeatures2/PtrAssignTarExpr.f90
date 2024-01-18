! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/OO_procptr/CrossFeatures2/PtrAssignTarExpr.f
! opt variations: -qnock -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: PtrAssignTarExpr.f 
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
!*  TEST CASE NAME             : PrtAssignTarExpr.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 14, 2005
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
!*  C726 (R742) An expr shall be a reference to a function whose result
!*  is a procedure pointer.
!* 
!* 
!*  (314528) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE :: DT(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: Str
    END TYPE

    INTERFACE
      FUNCTION FunI(Arg)
        IMPORT
        CLASS(DT(1,*))          :: Arg
        CLASS(DT(1,:)), POINTER :: FunI
      END FUNCTION
    END INTERFACE

    !ABSTRACT INTERFACE
    INTERFACE
      FUNCTION FunRetPtr1(Arg)
        IMPORT
        PROCEDURE(FunI), POINTER :: FunRetPtr1
        PROCEDURE(FunI)          :: Arg
      END FUNCTION
    END INTERFACE

    INTERFACE
      FUNCTION FunRetPtr2(Arg)
        IMPORT
        PROCEDURE(FunI), POINTER :: FunRetPtr2, Arg
      END FUNCTION
    END INTERFACE

  CONTAINS

    FUNCTION ModFun(Arg)
    PROCEDURE(FunI), POINTER :: ModFun
    PROCEDURE(FunI)          :: Arg
      PRINT *, "I like this!"
      ModFun => Arg
    END FUNCTION

    FUNCTION ModFun1(Arg)
    PROCEDURE(FunI), POINTER :: ModFun1, Arg
      PRINT *, "I like this too!"
      ModFun1 => Arg
    END FUNCTION

    FUNCTION FDT(Arg)
    CLASS(DT(1,*))          :: Arg
    CLASS(DT(1,:)), POINTER :: FDT
      ALLOCATE(FDT, SOURCE=Arg)
    END FUNCTION
  END MODULE


  PROGRAM PrtAssignTarExpr
  USE M
  IMPLICIT NONE

  PROCEDURE(FunRetPtr1), POINTER :: Ptr1
  PROCEDURE(FunRetPtr2), POINTER :: Ptr2
  PROCEDURE(FunI),       POINTER :: Ptr

    Ptr1  => ModFun
    Ptr   => Ptr1(FDT)
    SELECT TYPE ( As => Ptr(DT(1,3)("abc")))
    TYPE IS (DT(1,*))
      IF ( As%Str .NE. "abc" ) STOP 11
    CLASS DEFAULT
      STOP 12
    END SELECT

    Ptr2  => ModFun1
    Ptr   => Ptr2(Ptr1(FDT))
    SELECT TYPE ( As => Ptr(DT(1,3)("cba")))
    TYPE IS (DT(1,*))
      IF ( As%Str .NE. "cba" ) STOP 21
    CLASS DEFAULT
      STOP 22
    END SELECT


  END


