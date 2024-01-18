! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qdebug=intmsg
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp intrin_mod06.f
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For module nature in USE statement.
!*                             :
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  DIAGNOSES TESTED           : Cannot access an intrinsic module and
!*                               a nonintrinsic module with the same 
!*                               name in the same scope unit.
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

        module ieee_exceptions
          integer i
          contains
          subroutine sub()
            print*, "in ieee"
          end subroutine
        end module
       
        module mod1
          use, intrinsic :: ieee_exceptions
        end module

          use, non_intrinsic :: ieee_exceptions
          use mod1
        end
    
