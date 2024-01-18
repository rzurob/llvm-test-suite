!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxdmodprocstmt019.f
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
!*  TEST CASE TITLE            : fxdmodprocstmt019
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
!*  DESCRIPTION                : This test case checks the nesting of
!*                               interfaces and combination of proc
!*                               statements and proc declarations.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      ! explicit interface for aa
      interface 
         subroutine aa(arg)
           real :: arg
         end subroutine aa
      end interface



      interface gen1      ! the top level generic interface
         procedure aa     ! ok
         procedure() cc   ! illegal

         subroutine sub(a)

           ! explicit interface for ll
           interface
              subroutine ll(i)
                 logical :: i
              end subroutine ll
           end interface

           interface gen2     ! nested generic interface
              subroutine a()
                procedure() nn ! ok
                procedure xx   ! illegal
              end subroutine a
              procedure() ll   ! illegal
              procedure ll     ! ok
           end interface
           procedure(a) dd     ! ok
           procedure() ee      ! ok
           procedure ff        ! illegal
         end subroutine sub

      end interface
      
      end
