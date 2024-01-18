! *********************************************************************
!*
!*  ===================================================================
!*
!*  FUNCTIONALITY TESTED       : VALUE is filled with blanks if the
!*                             : specified environment variable does not
!*                             : exist. LENGTH is assigned 0.
!*                             : STATUS is assigned value 1.
!*
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
