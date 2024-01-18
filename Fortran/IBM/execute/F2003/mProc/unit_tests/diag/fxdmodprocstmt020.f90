!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxdmodprocstmt020.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : fxdmodprocstmt020
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Sept. 22, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : generalization of module procedure
!*                               stmts, by making the MODULE keyword
!*                               optional. These statements are called
!*                               procedure statements in F2003.
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : if identifier in a proc stmt does
!*                               not have an explicit interface
!*                               it should be flagged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      
      interface gen1
        procedure s1   ! illegal because s1 doesn't have explicit iface
      end interface gen1
      
      interface gen2
        procedure f1   ! illegal because f1 doesn't have explicit iface
      end interface gen2

      end

      subroutine s1()
        print*, "s1"
      end subroutine s1
      
      integer function f1(arg)
         integer :: arg
         f1 = arg
      end function f1
