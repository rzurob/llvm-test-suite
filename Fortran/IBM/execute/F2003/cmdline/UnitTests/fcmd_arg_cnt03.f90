! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=8 
! %GROUP: fcmd_arg_cnt03.f
! %VERIFY: 
! %STDIN: 
! %STDOUT: 
! %EXECARGS: aa "   " bb "" cc
! %POSTCMD: 
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : 
!*                             : 
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONALITIES TESTED     : For COMMAND_ARGUMENT_COUNT.
!*                             : It returns the number of the command
!*                             : line argument. A string of blanks or 
!*                             : an empty string (within double-quote)
!*                             : count as command line arguments.
!*                             : The command name itself does not count.
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

      Program fcmd_arg_cnt03
      
        integer :: i1
        i1 = command_argument_count()
        if (i1 .ne. 5) error stop 1_4
      
      End Program fcmd_arg_cnt03
