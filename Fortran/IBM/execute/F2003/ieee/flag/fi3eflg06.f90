! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:  -qfree=f90
! %GROUP: fi3eflg06.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
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
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing ieee_set_flag and ieee_get_flag
!*                               subroutines within external functions
!*                               (interface).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fi3eflg06

        use ieee_arithmetic

        interface

           logical function  set_flags_for_ieee_all_true()
             use ieee_arithmetic
           end function set_flags_for_ieee_all_true

           logical function set_flags_for_ieee_usual_true()
             use ieee_arithmetic
           end function set_flags_for_ieee_usual_true
        end interface

        logical :: all_flags_values(5),usual_flags_values(3)

!check if all flags are cleared
        call ieee_get_flag(ieee_all, all_flags_values)
        if (any(all_flags_values .neqv. .false.)) error stop 1


!test for IEEE_ALL array named constants
        all_flags_values = set_flags_for_ieee_all_true()
        if (any(all_flags_values .neqv. .true. )) error stop 4

!clear all flags
        call ieee_set_flag(ieee_all, .false. )
        call ieee_get_flag(ieee_all, all_flags_values)
        if (any(all_flags_values .neqv. .false. )) error stop 5

!set usual flags to true
        usual_flags_values = set_flags_for_ieee_usual_true()
        if (any(usual_flags_values .neqv. .true.)) error stop 8
!clear usual_flags
        call ieee_set_flag(ieee_usual, .false. )
        call ieee_get_flag(ieee_usual, usual_flags_values)
        if (any(usual_flags_values .neqv. .false. )) error stop 9

        end


        logical function set_flags_for_ieee_all_true()

          use ieee_exceptions
          logical*4 :: all_flags_values(5)

!check if all flags are cleared
          call ieee_get_flag(ieee_all, all_flags_values)
          if (any(all_flags_values .neqv. .false. )) error stop 2

          call ieee_set_flag(ieee_all, .true.)
          call ieee_get_flag(ieee_all, all_flags_values)
          if (any(all_flags_values .neqv. .true. )) error stop 3
          set_flags_for_ieee_all_true = all_flags_values(5)

        end function set_flags_for_ieee_all_true


        logical function set_flags_for_ieee_usual_true()

          use ieee_exceptions

          logical*4 :: usual_flags_values(3)

          call ieee_get_flag(ieee_usual, usual_flags_values)
          if (any(usual_flags_values .neqv. .false. )) error stop 6

          call ieee_set_flag(ieee_usual, .true.)
          call ieee_get_flag(ieee_usual, usual_flags_values)
          if (any(usual_flags_values .neqv. .true. )) error stop 7
          set_flags_for_ieee_usual_true = usual_flags_values(3)

        end function set_flags_for_ieee_usual_true

