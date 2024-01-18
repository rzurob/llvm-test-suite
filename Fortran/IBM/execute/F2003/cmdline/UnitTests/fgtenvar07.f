! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export EXIST_VAR=hello; export EMPTY_VAR= 
! %COMPOPTS: -qintsize=4 -qfixed
! %GROUP: fgtenvar07.f
! %VERIFY:
! %STDIN: 
! %STDOUT:
! %EXECARGS:
! %POSTCMD: unset EXIST_VAR; unset EMPTY_VAR 
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For GET_ENVIRONMENT_VARIABLE intrinsic.
!*                             : 
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONALITY TESTED       : VALUE takes the value of a specified
!*                             : environment variable. LENGTH is the
!*                             : length of the variable.
!*                             : STATUS is 0 when the length of VALUE
!*                             : is greater or equal to LENGTH.
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

      Program fgtenvar07
      
        integer :: i1, i2
        character(10) :: var_name
        character(5) :: var_value
        
        var_name = 'EXIST_VAR'
        
        call GET_ENVIRONMENT_VARIABLE(NAME=var_name, VALUE=var_value,
     +           LENGTH=i1, STATUS=i2)
        if (var_value .ne. "hello ") error stop 1
        if (i1 .ne. 5) error stop 2
        if (i2 .ne. 0) error stop 3
        
      End Program fgtenvar07