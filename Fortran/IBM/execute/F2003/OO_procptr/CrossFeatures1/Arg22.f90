! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Arg22.f 
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
!*  TEST CASE NAME             : Arg22.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 26, 2005
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
!*  Argument association - Implicit interface
!*  If the dummy argument is referenced as a subroutine, the actual argumenti
!*  shall be  a subroutine, subroutine procedure pointer, or dummy procedure.
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT TYPE(Base)(P)

    TYPE :: Base
      CHARACTER(3) :: C
    END TYPE
 
    INTERFACE
      SUBROUTINE IntF(Arg1, Arg2)
      IMPORT
        TYPE(Base), INTENT(IN)  :: Arg2 
        TYPE(Base), INTENT(OUT) :: Arg1 
      END SUBROUTINE 
    END INTERFACE

  CONTAINS

    SUBROUTINE ModSub1(Sub) 
    PROCEDURE(IntF) :: Sub 
      CALL modSub3(Sub, Sub) 
    END SUBROUTINE 

    SUBROUTINE ModSub2(Sub) 
    PROCEDURE() :: Sub 
      CALL ModSub4(Sub) 
    END SUBROUTINE 

 
    SUBROUTINE ModSub3(Proc0, Proc1)
    IMPLICIT TYPE(Base)(P)

    PROCEDURE(IntF) :: Proc0
    PROCEDURE()     :: Proc1
    TYPE(Base)      :: V

    CALL Proc0(V, Base("321"))
    IF (V%C .NE. "321") STOP 15

    CALL Proc1(V, Base("123"))
    IF (V%C .NE. "123") STOP 13

    END SUBROUTINE

    SUBROUTINE ModSub4(Proc)
    IMPLICIT TYPE(Base)(P)
    PROCEDURE()     :: Proc
    TYPE(Base)      :: V

    CALL Proc(V, Base("321"))
    IF (V%C .NE. "321") STOP 15

    END SUBROUTINE

  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(Base), INTENT(IN)  :: Arg2 
  TYPE(Base), INTENT(OUT) :: Arg1 
    Arg1 = Arg2
  END SUBROUTINE 


  PROGRAM Arg22
  USE M
  IMPLICIT TYPE(Base)(P)
  PROCEDURE(IntF) :: ExtSub 

  CALL ModSub1(ExtSub)
  CALL ModSub2(ExtSub)


  END

