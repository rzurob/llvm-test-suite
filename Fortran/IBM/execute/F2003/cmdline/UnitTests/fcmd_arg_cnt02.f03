! *********************************************************************
!*
!*  ===================================================================
!*
!*  FUNCTIONALITIES TESTED     : For COMMAND_ARGUMENT_COUNT.
!*                             : It returns the number of the command
!*                             : line argument (the command name itself
!*                             : does not count).
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fcmd_arg_cnt02

        integer :: i1
        i1 = command_argument_count()
        if (i1 .ne. 2) error stop 1

      End Program fcmd_arg_cnt02
