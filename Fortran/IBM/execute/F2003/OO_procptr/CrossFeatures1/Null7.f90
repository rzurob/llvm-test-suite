! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Null7.f 
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
!*  TEST CASE NAME             : Null7.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 11, 2005
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
!*   null()
!*   If any type parameters of the contextual entity are assumed,
!*   MOLD shall be present 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id=-1
      PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null7 
  USE M
  IMPLICIT NONE 
  TYPE(DT), POINTER :: V,W(:), U(:), X(:)

  INTERFACE ExtSub 
    SUBROUTINE ExtSub(V1, V2, V3, V4)
      IMPORt
      TYPE (DT), POINTER :: V1(:)
      TYPE (DT), POINTER :: V2(:)
      TYPE (DT), POINTER :: V3(:)
      TYPE (DT), POINTER :: V4(:)
    END SUBROUTINE
  END INTERFACE

  ALLOCATE(W(3))
  ALLOCATE(U(1:0))
  ALLOCATE(X(-1:0))
 
  CALL ExtSub( NULL(U),   &
             & NULL(W), &
             & NULL(X), &
             & X  )

 
  END


  SUBROUTINE ExtSub(V1, V2, V3, V4)
  USE M
  TYPE (DT), POINTER :: V1(:)
  TYPE (DT), POINTER :: V2(:)
  TYPE (DT), POINTER :: V3(:)
  TYPE (DT), POINTER :: V4(:)


  IF (ASSOCIATED(V1))  STOP 11
! IF (SIZE(V1) .NE. 0) STOP 12

  IF (ASSOCIATED(V2))  STOP 13
! IF (SIZE(V2) .NE. 0) STOP 14
  
  IF (ASSOCIATED(V3))  STOP 15
! IF (SIZE(V3) .NE. 0) STOP 16
  
  IF ( .NOT. ASSOCIATED(V4))         STOP 17
  IF (SIZE(V4)       .NE. 2)         STOP 18
  IF (ANY(LBOUND(V4) .NE. (/-1/)) )  sTOP 19 
  END SUBROUTINE



