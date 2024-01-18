!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ExecCmdLineWait.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
      program ExecCmdLineWait
        integer :: c

        call execute_command_line("echo wait", .FALSE., cmdstat = c)

        print *, c
      end

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Ren Jian Gang
!*  DATE                       : Dec 05, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!     No error condition occurs but WAIT is present with the value false,
!   XLF currently doesn't support asynchronous execution.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :
!*
!*
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
