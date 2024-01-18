!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxdmodprocstmt024.f
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
!*  TEST CASE TITLE            : fxdmodprocstmt024
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : April. 11, 2006
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
!*  DESCRIPTION                : A procedure pointer in the procedure dcl
!*                               stmt should be allowed in proc stmt if
!*                               it has an accessible explicit iface. There
!*                               should be no errors issued if the proc dcl
!*                               appears after the interface block.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      interface
         subroutine sub1()
         end subroutine
      end interface

      interface gen111
        procedure xxxx
      end interface 
      
      procedure(sub1), pointer :: xxxx
      
      procedure(sub1), pointer :: yyyy
      
      interface gen222
        procedure yyyy
      end interface

      xxxx => sub1
      yyyy => sub1
      
      
      call gen111()
      call gen222()
      

      end

      subroutine sub1()
        print*, "sub1"
      end subroutine
      
