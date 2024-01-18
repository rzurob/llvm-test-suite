!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxumodprocstmt012.f
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
!*  TEST CASE TITLE            : fxumodprocstmt012
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
!*  DESCRIPTION                : This functional test, makes sure that
!*                               if the identifier in a procedure stmt
!*                               refers to an external procedure, it 
!*                               behaves as expected, when it is called
!*                               using the generic interface in which
!*                               the proc stmt appears in.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      ! provide explicit ifaces for s1 and f1
      interface
         subroutine s1()
         end subroutine s1
      end interface
      interface
         integer function f1(arg)
           integer :: arg
         end function f1
      end interface
      
      ! the generic ifaces where proc stmts
      ! are present:
      interface gen1
        procedure s1
      end interface gen1
      
      interface gen2
        procedure f1
      end interface gen2

      ! call procs using generic ifaces:
      call gen1()
      print *, gen2(3)
      
      end

      subroutine s1()
        print*, "s1"
      end subroutine s1
      
      integer function f1(arg)
         integer :: arg
         f1 = arg
      end function f1
