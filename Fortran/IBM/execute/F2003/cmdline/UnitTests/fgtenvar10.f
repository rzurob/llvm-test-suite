! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export EXIST_VAR=hello; export EMPTY_VAR= 
! %COMPOPTS: -qintsize=8 -qfixed
! %GROUP: fgtenvar10.f
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
!*  FUNCTIONALITY TESTED       : VALUE is filled with blanks if the
!*                             : specified environment variable does not
!*                             : exist. LENGTH is assigned 0.
!*                             : STATUS is assigned value 1.
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

      Program fgtenvar10
      
        integer :: i1, i2
        character(10) :: var_name
        character(2) :: var_value
        
        var_name = 'EMPTY_VAR'
        
        call GET_ENVIRONMENT_VARIABLE('NONE_VAR', VALUE=var_value,
     +           LENGTH=i1, STATUS=i2)
        if (var_value .ne. "  ") error stop 1_4
        if (i1 .ne. 0) error stop 2_4
        if (i2 .ne. 1) error stop 3_4
        
      End Program fgtenvar10
