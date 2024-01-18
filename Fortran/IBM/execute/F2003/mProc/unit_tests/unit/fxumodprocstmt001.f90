!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxumodprocstmt001.f
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
!*  TEST CASE TITLE            : fxumodprocstmt001
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
!*                               a procedure names is not
!*                               module procedure, the MODULE keyword
!*                               is ignored, and the functionality will
!*                               be as expected.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      ! explicit interface for s1
      interface
         subroutine s1()
         end subroutine s1
         integer function f1()
         end function f1
      end interface

      interface generic_name1
         module procedure s1
      end interface

      interface generic_name2
         module procedure f1
      end interface

      call generic_name1()
      print *, generic_name2()

      end

      subroutine s1()
        print*, "s1"
      end subroutine s1

      integer function f1()
        f1 = 1
      end function
