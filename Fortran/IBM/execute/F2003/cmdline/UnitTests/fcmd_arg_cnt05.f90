! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=8
! %GROUP: fcmd_arg_cnt05.f
! %VERIFY:
! %STDIN: 
! %STDOUT:
! %EXECARGS: aa
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
!*                             : line argument (the command name itself
!*                             : does not count).
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

      Program fcmd_arg_cnt05
      
        integer(kind=2) :: i1
        i1 = command_argument_count()
        if (i1 .ne. 1) error stop 1_4
      
      End Program fcmd_arg_cnt05
