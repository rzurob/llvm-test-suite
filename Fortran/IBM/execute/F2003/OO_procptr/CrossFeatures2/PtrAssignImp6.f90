! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: PtrAssignImp6.f 
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
!*  TEST CASE NAME             : PtrAssignImp6.f 
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
!*   - Procedure 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    CONTAINS

    SUBROUTINE ModSub(Proc)
    PROCEDURE(INTEGER(1)) :: Proc
      IF ( Proc(-1_1) .NE. -1_1 ) STOP 11
    END SUBROUTINE
 
    FUNCTION Int1(Arg)
    INTEGER(1) :: Int1, Arg
    PROCEDURE(INTEGER(2)), POINTER :: ProcPtr
      Int1 = Arg
      ProcPtr => Int2
      IF ( ProcPtr(-2_2) .NE. -2_2 ) STOP 12 
    END FUNCTION

    FUNCTION Int2(Arg)
    INTEGER(2) :: Int2, Arg
    PROCEDURE(INTEGER(4)), POINTER :: ProcPtr
      Int2 = Arg 
      ProcPtr => Int4
      IF ( ProcPtr(-4_4) .NE. -4_4 ) STOP 14
    END FUNCTION

    FUNCTION Int4(Arg)
    INTEGER(4) :: Int4, Arg
    PROCEDURE(INTEGER(8)), POINTER  :: ProcPtr
      Int4 = Arg
      ProcPtr => Int8
      IF ( ProcPtr(-8_8) .NE. -8_8 ) STOP 18
    END FUNCTION

    FUNCTION Int8(Arg)
    INTEGER(8) :: Int8, Arg
    PROCEDURE(LOGICAL(1)), POINTER  :: ProcPtr
      Int8 = Arg
      ProcPtr => Log1 
      IF ( ProcPtr(.TRUE._1) .NEQV. .TRUE._1 ) STOP 21 
    END FUNCTION

   
    FUNCTION Log1(Arg)
    LOGICAL(1) :: Log1, Arg
    PROCEDURE(LOGICAL(2)), POINTER  :: ProcPtr
      Log1 = Arg
      ProcPtr => Log2 
      IF ( ProcPtr(.TRUE._2) .NEQV. .TRUE._2 ) STOP 22 
    END FUNCTION

    FUNCTION Log2(Arg)
    LOGICAL(2) :: Log2, Arg
    PROCEDURE(LOGICAL(4)), POINTER  :: ProcPtr
      Log2 = Arg
      ProcPtr => Log4 
      IF ( ProcPtr(.TRUE._4) .NEQV. .TRUE._4 ) STOP 24 
    END FUNCTION

    FUNCTION Log4(Arg)
    LOGICAL(4) :: Log4, Arg
    PROCEDURE(LOGICAL(8)), POINTER  :: ProcPtr
      Log4 = Arg
      ProcPtr => Log8 
      IF ( ProcPtr(.TRUE._8) .NEQV. .TRUE._8 ) STOP 28 
    END FUNCTION

    FUNCTION Log8(Arg)
    LOGICAL(8) :: Log8, Arg
      Log8 = Arg
    END FUNCTION


  END MODULE


  PROGRAM PtrAssignImp6
  USE M
  IMPLICIT NONE 

  CALL ModSub(Int1)

  END

