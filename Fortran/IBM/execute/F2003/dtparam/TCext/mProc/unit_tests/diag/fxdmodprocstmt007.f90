! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mProc/unit_tests/diag/fxdmodprocstmt007.f
! opt variations: -qnok -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxdmodprocstmt007.f
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
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if MODULE is not specified and the identifier
!*                               refers to a proc pointer component,
!*                               it is flagged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      interface
         subroutine sub_iface()
         end subroutine
         integer function fun_iface()
         end function
      end interface

      type a_type(k1)    ! (4)
          integer, kind :: k1
           procedure(sub_iface), nopass, pointer :: sub_ptr_comp
           procedure(fun_iface), nopass, pointer :: fun_ptr_comp
      end type a_type

      type(a_type(4)) :: x

      interface generic_name1
         procedure x%sub_ptr_comp
      end interface
      interface generic_name2
         procedure x%fun_ptr_comp
      end interface

      call generic_name1()
      call generic_name2()
      call x%sub_ptr_comp()
      print*, x%fun_ptr_comp()

      end

