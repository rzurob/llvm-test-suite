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
!* 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    CONTAINS

    FUNCTION Int1(Arg)
    INTEGER(1) :: Int1, Arg
      Int1 = Arg 
    END FUNCTION

    FUNCTION Int2(Arg)
    INTEGER(2) :: Int2, Arg
      Int2 = Arg 
    END FUNCTION

    FUNCTION Int4(Arg)
    INTEGER(4) :: Int4, Arg
      Int4 = Arg 
    END FUNCTION

    FUNCTION Int8(Arg)
    INTEGER(8) :: Int8, Arg
      Int8 = Arg 
    END FUNCTION

   
    FUNCTION Log1(Arg)
    LOGICAL(1) :: Log1, Arg
      Log1 = Arg
    END FUNCTION

    FUNCTION Log2(Arg)
    LOGICAL(2) :: Log2, Arg
      Log2 = Arg
    END FUNCTION

    FUNCTION Log4(Arg)
    LOGICAL(4) :: Log4, Arg
      Log4 = Arg
    END FUNCTION

    FUNCTION Log8(Arg)
    LOGICAL(8) :: Log8, Arg
      Log8 = Arg
    END FUNCTION


  END MODULE


  PROGRAM PtrAssignImp6
  USE M
  IMPLICIT NONE 


  PROCEDURE(Int1),   POINTER :: PtrInt1
  PROCEDURE(Int2),   POINTER :: PtrInt2
  PROCEDURE(Int4),   POINTER :: PtrInt4
  PROCEDURE(Int8),   POINTER :: PtrInt8

  PROCEDURE(Log1),   POINTER :: PtrLog1
  PROCEDURE(Log2),   POINTER :: PtrLog2
  PROCEDURE(Log4),   POINTER :: PtrLog4
  PROCEDURE(Log8),   POINTER :: PtrLog8

  PtrInt1 => Int1
  IF ( PtrInt1(-1_1) .NE. -1_1 ) STOP 11

  PtrInt2 => Int2
  IF ( PtrInt2(-2_2) .NE. -2_2 ) STOP 12

  PtrInt4 => Int4
  IF ( PtrInt4(-4_4) .NE. -4_4 ) STOP 14

  PtrInt8 => Int8
  IF ( PtrInt8(-8_8) .NE. -8_8 ) STOP 18


  PtrLog1 => Log1
  IF ( PtrLog1(.TRUE._1) .NEQV. .TRUE._1 ) STOP 21

  PtrLog2 => Log2
  IF ( PtrLog2(.TRUE._2) .NEQV. .TRUE._2 ) STOP 22

  PtrLog4 => Log4
  IF ( PtrLog4(.TRUE._4) .NEQV. .TRUE._4 ) STOP 24

  PtrLog8 => Log8
  IF ( PtrLog8(.FALSE._8) .NEQV. .FALSE._8 ) STOP 28


  END

