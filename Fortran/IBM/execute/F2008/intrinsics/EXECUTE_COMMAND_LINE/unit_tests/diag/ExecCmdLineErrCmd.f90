!************************************************************************
      program ExecCmdLineErrCmd
        call execute_command_line("./null.out", .TRUE.)

        print *, "We should not come here:)"
      end

!*  ===================================================================
!*
!*  DATE                       : Dec 05, 2010
!*                             : IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Error command executed
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
