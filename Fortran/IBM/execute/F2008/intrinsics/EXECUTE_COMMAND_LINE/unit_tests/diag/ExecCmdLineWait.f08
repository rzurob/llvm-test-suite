!************************************************************************
      program ExecCmdLineWait
        integer :: c

        call execute_command_line("echo wait", .FALSE., cmdstat = c)

        print *, c
      end

!*  ===================================================================
!*
!*  DATE                       : Dec 05, 2010
!*                             : IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!     No error condition occurs but WAIT is present with the value false,
!   XLF currently doesn't support asynchronous execution.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890