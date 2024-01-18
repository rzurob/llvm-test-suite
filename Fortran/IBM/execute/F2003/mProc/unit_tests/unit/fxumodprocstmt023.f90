!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxumodprocstmt023.f
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
!*  TEST CASE TITLE            : fxumodprocstmt023
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
!*                               a module procedure can appear in 
!*                               more than one interface blocks, that 
!*                               have different identifiers, and 
!*                               are in the same scope.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      module m
      implicit none
      
      interface gen1
        procedure s1
      end interface
      interface gen2
        procedure s1
      end interface
      interface gen3
        module procedure s1
      end interface
      interface gen4
        module procedure s1
      end interface
      
      contains
      subroutine s1()
        print *, "s1"
      end subroutine
      
      end module m


      use m
      
      call gen1()
      call gen2()
      call gen3()
      call gen4()
      

      end
