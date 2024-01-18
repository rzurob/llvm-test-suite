!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxumodprocstmt016.f
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
!*                               refers to a procedure pointer, it
!*                               behaves as expected. In this test case
!*                               the MODULE keyword is specified, and
!*                               the expected behaviour is to ignore
!*                               this keyword when possible to recover
!*                               from a mistake made by the user.
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


      procedure(s1), pointer :: sub_ptr
      procedure(f1), pointer :: fun_ptr

      ! the generic ifaces where proc stmts
      ! are present:
      interface gen1
        module procedure sub_ptr
      end interface gen1

      interface gen2
        module procedure fun_ptr
      end interface gen2

      sub_ptr => s1
      fun_ptr => f1

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
