! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=4 -qnullterm -qfixed
! %GROUP: fgtenvar11.f
! %VERIFY:
! %STDIN: 
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
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
!*  FUNCTIONALITY TESTED       : When TRIM_NAME presents and has value
!*                             : false, the trailing blanks in NAME are
!*                             : significant. Otherwise, thery are not
!*                             : part of the environment variable's name.
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

      Program fgtenvar11
      
        integer :: i1, i2
        character(7) :: var_value, ch1
        logical :: false /.false./
        logical :: true  /.true./
        
        call setenv('HAS_BLANK', 'noblank', 0)        
        call GET_ENVIRONMENT_VARIABLE('HAS_BLANK', VALUE=var_value,
     +         LENGTH=i1, STATUS=i2)
        if (var_value .ne. "noblank") error stop 1
        if (i1 .ne. 7) error stop 2
        if (i2 .ne. 0) error stop 3
        
        call setenv("HAS_BLANK   ", "blank", 0)
        call GET_ENVIRONMENT_VARIABLE("HAS_BLANK   ", VALUE=var_value,
     +         LENGTH=i1, STATUS=i2, TRIM_NAME=false)
        if (var_value .ne. "blank  ") error stop 4
        if (i1 .ne. 5) error stop 5
        if (i2 .ne. 0) error stop 6
        
        call GET_ENVIRONMENT_VARIABLE('HAS_BLANK   ', VALUE=var_value,
     +         LENGTH=i1, STATUS=i2, TRIM_NAME=true)
        if (var_value .ne. "noblank") error stop 7
        if (i1 .ne. 7) error stop 8
        if (i2 .ne. 0) error stop 9
        
      End Program fgtenvar11