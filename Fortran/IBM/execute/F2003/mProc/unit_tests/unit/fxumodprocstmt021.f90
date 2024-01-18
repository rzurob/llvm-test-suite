!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxumodprocstmt021.f
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
!*  TEST CASE TITLE            : fxumodprocstmt021
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
!*                               a procedure pointer can appear in 
!*                               more than one interface blocks, that 
!*                               have different identifiers, and 
!*                               are in the same scope.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      interface
         subroutine s1()
         end subroutine s1
      end interface
      
      procedure(s1), pointer :: proc_ptr

      interface gen1
        procedure proc_ptr
      end interface
      interface gen2
        procedure proc_ptr
      end interface
      interface gen3
        module procedure proc_ptr
      end interface
      interface gen4
        module procedure proc_ptr
      end interface

      proc_ptr => s1
      
      call gen1()
      call gen2()
      call gen3()
      call gen4()


      end

      subroutine s1()
        print *, "s1"
      end subroutine s1
      
