!************************************************************************
      program ExecCmdLineLanglvl
        call execute_command_line("ls")
      end

!*  ===================================================================
!*
!*  DATE                       : Dec 06, 2010
!*                             : IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!     A message is emitted for langlvl error for execute_command_line intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :  -qlanglvl=2003std
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
