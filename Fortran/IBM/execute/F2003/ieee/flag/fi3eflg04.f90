! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:  -qfree=f90
! %GROUP: fi3eflg04.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Vasile Radulescu
!*  DATE                       : February 15, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_FLAG
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_FLAG
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : SUBROUTINE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing ieee_set_flag and ieee_get_flag
!*                               subroutines within internal subroutines.
!*                               
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fi3eflg04

        use ieee_arithmetic

        logical :: all_flag_values(5), usual_flag_values(3)
        logical :: expected_all_flags(5), expected_usual_flags(3)


!check if all flags are cleared
        call ieee_get_flag(ieee_all, all_flag_values)
        if ( any(all_flag_values .neqv. .false. )) error stop 1


!set all flags in IEEE_ALL array to true calling an internal subroutine
        call set_flags_for_ieee_all_true()
        call ieee_get_flag(ieee_all, all_flag_values)
        if (any(all_flag_values .neqv. .true. )) error stop 2

!set back all flags to false 
        call ieee_set_flag(ieee_all, .false. )
        call ieee_get_flag(ieee_all, all_flag_values)
        if (any(all_flag_values .neqv. .false. )) error stop 3

!set  values in the array IEEE_USUAL  to true calling second internal subroutine
        call set_flags_for_ieee_usual_true()
        call ieee_get_flag(ieee_usual, usual_flag_values )
        if ( any(usual_flag_values .neqv. .true. )) error stop 4

!set values in the array IEEE_USUAL  to false 
        call ieee_set_flag(ieee_usual,  .false. ) 
        call ieee_get_flag( ieee_usual, usual_flag_values )
        if ( any( usual_flag_values .neqv. .false. )) error stop 5

        contains

        subroutine set_flags_for_ieee_all_true()
           use ieee_exceptions
           logical*4 all_flag_values(5)
           call ieee_get_flag(ieee_all, all_flag_values)
        !....check if all flags are cleared when entering procedure.
           if (any(all_flag_values  .neqv. .false. )) error stop 6
        !....check if all flags are set when exiting procedure
           call ieee_set_flag(ieee_all, .true. )
           call ieee_get_flag(ieee_all, all_flag_values)
           if (any(all_flag_values .neqv. .true. )) error stop 6
        end subroutine


        subroutine set_flags_for_ieee_usual_true()
           use ieee_exceptions

           logical*4 usual_flag_values(3)

           call ieee_get_flag(ieee_usual, usual_flag_values)
           
           !....check if usual flags are cleared when entering procedure.
           if (any(usual_flag_values .neqv. .false. )) error stop 10
           
           !....check if usual flags are set when exiting procedure
           call ieee_set_flag(ieee_usual, .true. )
           call ieee_get_flag(ieee_usual, usual_flag_values)
           if (any(usual_flag_values .neqv. .true. )) error stop 11

        end subroutine

        end program
 
