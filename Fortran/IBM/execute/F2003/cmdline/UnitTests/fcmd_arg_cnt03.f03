! *********************************************************************
!*
!*  ===================================================================
!*
!*  FUNCTIONALITIES TESTED     : For COMMAND_ARGUMENT_COUNT.
!*                             : It returns the number of the command
!*                             : line argument. A string of blanks or
!*                             : an empty string (within double-quote)
!*                             : count as command line arguments.
!*                             : The command name itself does not count.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fcmd_arg_cnt03

        integer :: i1
        i1 = command_argument_count()
        if (i1 .ne. 5) error stop 1_4

      End Program fcmd_arg_cnt03