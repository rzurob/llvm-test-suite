!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxumodprocstmt006.f
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
!*  TEST CASE TITLE            : fxumodprocstmt006
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
!*                               if the MODULE keyword is not specified and
!*                               if an identifier in a proc stmt refers
!*                               to an internal procedure, it is ignored
!*                               and the functionality of the rest of the
!*                               program is not affected.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      interface
         subroutine s1()
         end subroutine
      end interface
      
      call s1()

      end
      
      subroutine s1()
      implicit none

        interface
           integer function f2(arg)
             integer :: arg
           end function
        end interface
      
        interface gen
          procedure ff, f2
        end interface

        print *, gen(3)      

        contains
        integer function ff()
          ff = 1
        end function ff
      end subroutine s1

      integer function f2(arg)
        integer :: arg
        f2 = arg
      end function f2
