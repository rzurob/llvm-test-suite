!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxumodprocstmt002.f
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
!*  TEST CASE TITLE            : fxumodprocstmt002
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
!*                               if MODULE keyword is specified and
!*                               the procedure names is a procedure-
!*                               pointer, MODULE is ignored and the
!*                               functionality is intact.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      ! explicit interface for s1
      interface
         subroutine s1()
         end subroutine s1
      end interface

      ! explicit interface for f1
      interface
         integer function f1()
         end function f1
      end interface

      procedure(s1), pointer :: ptr1
      procedure(f1), pointer :: ptr2

      interface generic_name1
         module procedure ptr1
      end interface
      interface generic_name2
         module procedure ptr2
      end interface

      ptr1 => s1
      ptr2 => f1

      call generic_name1()
      print *, generic_name2()

      end

      subroutine s1()
        print*, "s1"
      end subroutine s1
      integer function f1()
         f1 = 1
      end function f1
