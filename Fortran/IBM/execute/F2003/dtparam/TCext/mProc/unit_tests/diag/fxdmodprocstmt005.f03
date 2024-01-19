! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mProc/unit_tests/diag/fxdmodprocstmt005.f
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
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if MODULE is not specified and the identifier
!*                               refers to a type-bound proc, it is flagged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module m
      implicit none

      type a_type(k1)    ! (4)
          integer, kind :: k1
        contains
           procedure, nopass :: type_bound_sub
           procedure, nopass :: type_bound_fun
      end type a_type

      type(a_type(4)) x

      interface generic_name1
         procedure x%type_bound_sub
      end interface
      interface generic_name2
         procedure x%type_bound_fun
      end interface

      contains
      subroutine type_bound_sub()
        print*, "sub tbp called"
      end subroutine
      integer function type_bound_fun()
        type_bound_fun = 1
      end function

      end module m

      use m

      interface gen1
          procedure x%type_bound_sub
      end interface gen1
      interface gen2
          procedure x%type_bound_sub
      end interface gen2

      call generic_name1()
      call generic_name2()
      call x%type_bound_sub()
      print*, x%type_bound_fun()

      end

