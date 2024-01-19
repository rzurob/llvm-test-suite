! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_FLAG
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_FLAG
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SUBROUTINE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing ieee_set_flag and ieee_get_flag
!*                               subroutines within external subroutine.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fi3eflg05

        use ieee_exceptions

        logical :: all_flag_values(5), usual_flag_values(3)

!check if all flags are cleared
        call ieee_get_flag(ieee_all, all_flag_values )
        if (any(all_flag_values .neqv. .false. )) error stop 1

!call external subroutine
        call test_set_flag_for_ieee_all
        call ieee_get_flag(ieee_all, all_flag_values)
        if (any(all_flag_values .neqv. .true. )) error stop 4

!clear all flags
        call ieee_set_flag(ieee_all, .false. )
        call ieee_get_flag(ieee_all, all_flag_values)
        if (any(all_flag_values .neqv. .false. )) error stop 5

!call external subroutine
        call test_set_flag_for_ieee_usual
        call ieee_get_flag(ieee_usual, usual_flag_values)
        if (any(usual_flag_values .neqv. .true. )) error stop 8

!clear the flags
        call ieee_set_flag( ieee_usual, .false. )
        call ieee_get_flag(ieee_usual, usual_flag_values)
        if (any(usual_flag_values .neqv. .false. )) error stop 9

        end

        subroutine test_set_flag_for_ieee_all
!test for IEEE_ALL array named constants
        use ieee_exceptions
        logical :: all_flag_values(5)

!check if all flags are cleared
        call ieee_get_flag(ieee_all, all_flag_values)
        if (any(all_flag_values .neqv. .false.)) error stop 2

!set all flags to true
        call ieee_set_flag(ieee_all, .true.)
        call ieee_get_flag(ieee_all, all_flag_values)

        if (any(all_flag_values .neqv. .true.)) error stop 3

        end subroutine


        subroutine test_set_flag_for_ieee_usual

!test for IEEE_USUAL array named constants
        use ieee_exceptions
        logical :: usual_flag_values(3)

!check if usual flags are cleared
        call ieee_get_flag(ieee_usual, usual_flag_values)
        if ( any(usual_flag_values .neqv. .false. )) error stop 6

!set all usual flags to true
        call ieee_set_flag(ieee_usual, .true.)
        call ieee_get_flag(ieee_usual, usual_flag_values)
        if (any(usual_flag_values .neqv. .true. )) error stop 7

        end subroutine


