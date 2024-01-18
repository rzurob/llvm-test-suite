! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export EXIST_VAR=hello; export EMPTY_VAR=
! %COMPOPTS: -qintsize=2 -qfixed
! %GROUP: fgtenvar09.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: unset EXIST_VAR; unset EMPTY_VAR
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  FUNCTIONALITY TESTED       : VALUE is filled with blanks if the
!*                             : specified environment variable does not
!*                             : have a value. LENGTH is assigned 0.
!*                             : STATUS is 0.
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgtenvar09

        integer :: i1, i2
        character(10) :: var_name
        character(2) :: var_value

        var_name = 'EMPTY_VAR'

        call GET_ENVIRONMENT_VARIABLE(NAME=var_name, VALUE=var_value,
     +            LENGTH=i1, STATUS=i2)
        if (var_value .ne. "  ") error stop 1
        if (i1 .ne. 0) error stop 2
        if (i2 .ne. 0) error stop 3

      End Program fgtenvar09