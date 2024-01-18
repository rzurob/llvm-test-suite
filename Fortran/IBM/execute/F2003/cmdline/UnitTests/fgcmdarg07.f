! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=8 -qfixed
! %GROUP: fgcmdarg07.f
! %VERIFY:
! %STDIN: 
! %STDOUT:
! %EXECARGS: aa bbb cccc
! %POSTCMD:
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For GET_COMMAND_ARGUMENT intrinsic.
!*                             : 
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONALITY TESTED       : VALUE takes the value of a specified
!*                             : command line argument. LENGTH is the
!*                             : length of the argument. STATUS is 0
!*                             : when the length of VALUE is greater
!*                             : or equal to LENGTH, and -1 otherwise.
!*                             : STATUS is assigned value 1 if the
!*                             : retrieval fails.
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmdarg07
      
        integer :: i1, i2, i3
        character(3) :: ch1
        
        i1=1
        call GET_COMMAND_ARGUMENT(NUMBER=i1, VALUE=ch1, LENGTH=i2,
     +           STATUS=i3)
        if (ch1 .ne. "aa ") error stop 1_4
        if (i2 .ne. 2) error stop 2_4
        if (i3 .ne. 0) error stop 3_4
        
        i1 = 3
        call GET_COMMAND_ARGUMENT(NUMBER=i1, VALUE=ch1, LENGTH=i2,
     +           STATUS=i3)
        if (ch1 .ne. "ccc") error stop 4_4
        if (i2 .ne. 4) error stop 5_4
        if (i3 .ne. -1) error stop 6_4
      
      End Program fgcmdarg07
