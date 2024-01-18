! *********************************************************************
!*
!*  ===================================================================
!*
!*  FUNCTIONALITY TESTED       : VALUE takes the value of a specified
!*                             : environment variable. LENGTH is the
!*                             : length of the variable.
!*                             : STATUS is -1 when the length of VALUE
!*                             : is less than LENGTH.
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgtenvar08

        integer :: i1, i2
        character(10) :: var_name
        character(2) :: var_value

        var_name = 'EXIST_VAR'

        call GET_ENVIRONMENT_VARIABLE(NAME=var_name, VALUE=var_value,
     +           LENGTH=i1, STATUS=i2)
        !if (var_value .ne. "he") error stop 1_4
        print*, var_value
        ! if (i1 .ne. 5) error stop 2_4
        print*, i1
        ! if (i2 .ne. -1) error stop 3_4
        print*, i2

      End Program fgtenvar08
