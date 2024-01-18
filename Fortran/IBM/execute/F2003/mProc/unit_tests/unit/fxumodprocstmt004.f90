!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxumodprocstmt004.f
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
!*  REQUIRED COMPILER OPTIONS  : -qdebug=ooall
!*
!*  DESCRIPTION                : This functional test, makes sure that
!*                               if the MODULE keyword is not specified and
!*                               if an identifier in a proc stmt refers
!*                               to a type-bound proc, it is ignored
!*                               and the functionality of the rest of the
!*                               program is not affected.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      ! explicit interface for s1
      interface
         subroutine s1(arg)
           integer :: arg
         end subroutine s1
         integer function f1()
         end function f1
         subroutine tbp
         end subroutine tbp
      end interface

      type mytype
        contains
         procedure, nopass :: tbp
      end type

      type(mytype) xx

      interface generic_name1
         procedure xx%tbp
         procedure s1, tbp
      end interface

      interface generic_name2
         procedure xx%tbp
         procedure f1
      end interface


      call generic_name1()
      call generic_name1(1)
      print *, generic_name2()

      end

      subroutine s1(arg)
        integer :: arg
        print*, "s1"
      end subroutine s1

      integer function f1()
        f1 = 1
      end function

      subroutine tbp()
        print *, "tbp"
      end subroutine
