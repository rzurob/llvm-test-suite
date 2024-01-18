! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=4
! %GROUP: fgcmd09.f
! %VERIFY:
! %STDIN: 
! %STDOUT:
! %EXECARGS: aa bbb
! %POSTCMD:
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For GET_COMMAND intrinsic.
!*                             : 
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONALITY TESTED       : STATUS is assigned value -1 if the 
!*                             : length of COMMAND is less than the 
!*                             : length of the argument regardless
!*                             : the absence of LENGTH.
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : The executable is named ./file
!*                             : It is ./fgcmd09 for this test case.
!*                             : It affects the LENGTH of the COMMAND
!*                             : if the name of the file is changed.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmd09
      
        integer :: i1
        character(len=14) :: ch1
        
        call GET_COMMAND(COMMAND=ch1, STATUS=i1)

        if (ch1 .ne. "./fgcmd09 aa b") error stop 1
        if (i1 .ne. -1) error stop 2
      
      End Program fgcmd09