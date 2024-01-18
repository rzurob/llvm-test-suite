! *********************************************************************
!*
!*  ===================================================================
!*
!*  FUNCTIONALITY TESTED       : Test that a integer var can be used
!*                             : as logical expression when -qintlog
!*                             : is specified.
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgtenvar12

        integer :: i1, i2, i3, i4
        character(7) :: var_value
        logical :: false, true
        false = .false.
        true = .true.
        i3 = 3
        i4 = 0

        call setenv('HAS_BLANK', 'noblank', 0)
        call GET_ENVIRONMENT_VARIABLE('HAS_BLANK', VALUE=var_value,
     +         LENGTH=i1, STATUS=i2)
        if (var_value .ne. "noblank") error stop 1
        if (i1 .ne. 7) error stop 2
        if (i2 .ne. 0) error stop 3

        call setenv('HAS_BLANK   ', 'blank', 0)
        call GET_ENVIRONMENT_VARIABLE('HAS_BLANK   ', VALUE=var_value,
     +         LENGTH=i1, STATUS=i2, TRIM_NAME=i4)
        if (var_value .ne. "blank  ") error stop 4
        if (i1 .ne. 5) error stop 5
        if (i2 .ne. 0) error stop 6

        call GET_ENVIRONMENT_VARIABLE('HAS_BLANK   ', VALUE=var_value,
     +         LENGTH=i1, STATUS=i2, TRIM_NAME=i3)
        if (var_value .ne. "noblank") error stop 7
        if (i1 .ne. 7) error stop 8
        if (i2 .ne. 0) error stop 9

      End Program fgtenvar12