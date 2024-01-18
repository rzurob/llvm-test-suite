!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxdmodprocstmt015.f
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
!*  TEST CASE TITLE            : fxdmodprocstmt015
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
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=90std
!*
!*  DESCRIPTION                : If the ordering of interface body
!*                               and procedure statements do not 
!*                               conform to the F90 std, then we should
!*                               NOT flag the ordering if 'MODULE'
!*                               keyword is not specified, since another
!*                               'L' level message is given for that.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      interface 
         subroutine s1()
         end subroutine s1
         subroutine s3(arg)
           real :: arg
         end subroutine s3
      end interface

      interface gen1
        procedure s1
        subroutine s2(arg)
          integer :: arg
        end subroutine s2
      end interface

      interface gen2
        module procedure s1
        subroutine s4(arg)
          integer :: arg
        end subroutine s4
      end interface

      interface gen3
        module procedure s1
        procedure s3
        subroutine s5(arg)
          integer :: arg
        end subroutine s5
      end interface

      interface gen4
        procedure s3
        module procedure s1
        subroutine s6(arg)
          integer :: arg
        end subroutine s6
      end interface
      
      end
