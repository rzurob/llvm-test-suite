! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mProc/unit_tests/unit/fxumodprocstmt005.f
! opt variations: -qnok -ql

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
!*                               if the MODULE keyword is not specified and
!*                               if an identifier in a proc stmt refers
!*                               to a proc pointer component, it is ignored
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

      type mytype(k1)    ! (4)
         integer, kind :: k1
         procedure(s1), nopass, pointer :: proc_ptr
      end type

      type(mytype(4)) xx

      interface generic_name1
         procedure xx%proc_ptr
         procedure s1
      end interface

      interface generic_name2
         procedure xx%proc_ptr
         procedure f1
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
