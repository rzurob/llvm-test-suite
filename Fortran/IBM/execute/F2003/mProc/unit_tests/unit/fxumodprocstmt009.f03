!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 22, 2005
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
!*                               if the MODULE keyword is specified and
!*                               if an identifier in a proc stmt refers
!*                               to a proc pointer component, both the
!*                               identifier and the MODULE keyword are ignored
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
      end interface

      type mytype
         procedure(s1), nopass, pointer :: proc_ptr
      end type

      type(mytype) xx

      interface generic_name1
         module procedure xx%proc_ptr
         module procedure s1
      end interface

      interface generic_name2
         module procedure xx%proc_ptr
         module procedure f1
      end interface

      xx%proc_ptr => s1

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
