!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxumodprocstmt019.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 22, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : generalization of module procedure
!*                               stmts, by making the MODULE keyword
!*                               optional. These statements are called
!*                               procedure statements in F2003.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This functional test, makes sure that
!*                               if the identifier in a procedure stmt
!*                               refers to a module procedure, it
!*                               behaves as expected, when it is called
!*                               using the generic interface in which
!*                               the proc stmt appears in. MODULE keyword
!*                               is specified in this test case.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module m
      implicit none

      interface gen1
         module procedure s1
      end interface
      interface gen2
         module procedure f1
      end interface

      contains
      subroutine s1()
        print*, "s1"
      end subroutine s1

      integer function f1(arg)
         integer :: arg
         f1 = arg
      end function f1


      end module m

      use m

      call gen1()
      print *, gen2(3)

      end
