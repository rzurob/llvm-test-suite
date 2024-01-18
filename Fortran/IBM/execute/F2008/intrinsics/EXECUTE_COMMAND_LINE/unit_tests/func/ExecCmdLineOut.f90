!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ExecCmdLineOut.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Ren Jian Gang
!*  DATE                       : December 05, 2010
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic function
!*                               execute_command_line(command,wait,exitstat,cmdstat,cmdmsg)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 381472
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Test functionality of execute_command_line intrinsic with different
!*  combinations of arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program ExecCmdLineOut
  logical :: wait_arg = .TRUE.
  integer :: exitstat_arg
  integer :: cmdstat_arg
  character(9) :: cmdmsg_arg = "No Error!"

  call execute_command_line("echo First Call")

  call execute_command_line("echo Second Call", .TRUE.)

  call execute_command_line("echo Third Call", wait_arg)

  call execute_command_line("echo 4th Call", wait_arg, exitstat_arg)
  print *, exitstat_arg

  call execute_command_line("echo 5th Call", wait_arg, exitstat_arg, &
    cmdstat_arg)
  print *, exitstat_arg, cmdstat_arg

  call execute_command_line("echo 6th Call", wait_arg, exitstat_arg, &
    cmdstat_arg, cmdmsg_arg)
  print *, exitstat_arg, cmdstat_arg, cmdmsg_arg

  call execute_command_line("echo 7th Call", exitstat = exitstat_arg, &
    cmdstat = cmdstat_arg, cmdmsg = cmdmsg_arg)
  print *, exitstat_arg, cmdstat_arg, cmdmsg_arg

  call execute_command_line("echo 8th Call", cmdstat = cmdstat_arg, &
    cmdmsg = cmdmsg_arg)
  print *, cmdstat_arg, cmdmsg_arg

  call execute_command_line("echo 9th Call", cmdmsg = cmdmsg_arg)
  print *, cmdmsg_arg

  call execute_command_line("echo 10th Call", wait = .TRUE., cmdstat = cmdstat_arg)
  print *, cmdstat_arg

  call execute_command_line("echo 11th Call", wait = .TRUE., cmdstat = cmdstat_arg, &
    cmdmsg = cmdmsg_arg)
  print *, cmdstat_arg, cmdmsg_arg
end