! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Assign5.f
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
!*  TEST CASE NAME             : Assign5.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 16, 2005
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
!*  304717->305713 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE 
      FUNCTION CToC(Arg)
        CHARACTER(*) :: Arg(:)
        CHARACTER(LEN(Arg)) :: CToC(SIZE(Arg))
      END FUNCTION
    END INTERFACE

    TYPE :: Base
      PROCEDURE(CToC), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT
      INTEGER :: Id
      TYPE(Base), ALLOCATABLE :: BComp
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg(:)
    CHARACTER(LEN(Arg)) :: Fun(SIZE(Arg))
      Fun = Arg
    END FUNCTION

    RECURSIVE FUNCTION SavePtr(Arg1, Arg2) RESULT(Res)
    PROCEDURE(CToC)                :: Arg1
    CHARACTER(*)                   :: Arg2(:)
    PROCEDURE(CToC), POINTER, SAVE :: ProcPtr=>NULL()
    INTEGER,                  SAVE :: Count=5
    CHARACTER(LEN(Arg2))           :: Res(SIZE(Arg2))

      Count = Count - 1
      ProcPtr => Arg1

      IF (Count .NE. 0 ) THEN
        Res=SavePtr(Arg1,Arg2)
      ELSE
        IF ( .NOT. ASSOCIATED(ProcPtr, Arg1) ) THEN
          Res=""
        ELSE
          Res=Arg1(Arg2)
          Count = 5 ! restore the init
        END IF
      END IF
 
    END FUNCTION
    
     
  END MODULE


  PROGRAM Assign5 
  USE M
  IMPLICIT NONE 

  TYPE (DT) :: V
  PROCEDURE(SavePtr), POINTER :: ProcPtr
  CHARACTER(1025) :: Str=CHAR(40)

  ALLOCATE(V%BComp)
  V%BComp = Base(RetPtr(Fun))

  IF ( .NOT. ALLOCATED(V%BComp) )  STOP 12
  IF ( .NOT. ASSOCIATED(V%BComp%ProcPtr, RetPtr(Fun)) ) STOP 13

  ProcPtr => SavePtr 
  IF (ANY(ProcPtr(Fun, (/"123", "abc"/)) .NE. (/"123", "abc"/)) ) STOP 14
  IF (ANY(ProcPtr(Fun, (/"", ""/))       .NE. (/"", ""/)) )       STOP 15


  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(CToC), POINTER :: RetPtr
  PROCEDURE(CToC) :: Arg 
    RetPtr => Arg 
  END FUNCTION

  END

