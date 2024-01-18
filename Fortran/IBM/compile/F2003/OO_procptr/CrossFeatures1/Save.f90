! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp Save.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Save.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 07, 2005
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
!*  Save 
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Save 
  IMPLICIT TYPE(Base)(P)

  TYPE :: Base
    CHARACTER(3) :: C
  END TYPE

  CONTAINS

    SUBROUTINE IntSub1(Proc)
    SAVE        :: Proc
    PROCEDURE() :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub2(Proc)
    SAVE        :: Proc
    EXTERNAL    :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub3(Proc)
    SAVE                  :: Proc
    PROCEDURE(TYPE(BASE)) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub4()
    EXTERNAL    :: Proc
    SAVE        :: Proc
    END SUBROUTINE

  END


