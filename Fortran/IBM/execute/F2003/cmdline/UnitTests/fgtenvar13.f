! *********************************************************************
!*
!*  ===================================================================
!*
!*  FUNCTIONALITY TESTED       : Optional args do not appear
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgtenvar13

        character(10) :: var_name
        character(2) :: var_value

        var_name = 'HOME'

        call GET_ENVIRONMENT_VARIABLE(NAME=var_name, VALUE=var_value)

      End Program fgtenvar13