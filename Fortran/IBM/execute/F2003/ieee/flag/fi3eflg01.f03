! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_FLAG
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_FLAG
!*
!*  REQUIRED COMPILER OPTIONS  : -qintsize=2
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing ieee_set_flag and ieee_get_flag
!*                               subroutines for both arrays named constants
!*                               IEEE_USUAL and IEEE_ALL, for LOGICAL(2)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fi3eflg01

        use ieee_arithmetic

        logical :: all_flags(5), usual_flags(3)
        logical :: expected_all_flags(5), expected_usual_flags(3)

        integer :: k


!...test IEEE_ALL for true
!set all flags for IEEE_ALL array named constants to true
        call ieee_set_flag(ieee_all, .true.)

!check if all flags for IEEE_ALL array named constants are set to true
        call ieee_get_flag(ieee_all, all_flags)
        expected_all_flags  = (/.true., .true., .true., .true., .true./)
        do k = 1, 5
          if (all_flags(k) .neqv. expected_all_flags(k)) then
            error stop 1
          endif
        enddo

!check if either all flags for IEEE_USUAL array named constants are set to true
        call ieee_get_flag(ieee_usual, usual_flags)
        expected_usual_flags  = (/.true., .true., .true. /)
        do k = 1, 3
          if (usual_flags(k) .neqv. expected_usual_flags(k)) then
            error stop 2
          endif
        enddo

!...test IEEE_ALL for false
!set all flags for IEEE_ALL to false
        call ieee_set_flag(ieee_all, .false.)

!check if all flags for IEEE_ALL array named constants are set to false
        call ieee_get_flag(ieee_all, all_flags)
        expected_all_flags  = (/.false., .false., .false., .false., .false./)
        do k = 1, 5
          if (all_flags(k) .neqv. expected_all_flags(k)) then
            error stop 3
          endif
        enddo

!check if either all flags for IEEE_USUAL array named constants are set to false
        call ieee_get_flag(ieee_usual, usual_flags)
        expected_usual_flags  = (/.false., .false., .false. /)
        do k = 1, 3
          if (usual_flags(k) .neqv. expected_usual_flags(k)) then
            error stop 4
          endif
        enddo

!...test IEEE_USUAL for true
!set all flags for IEEE_USUAL array named constants to true
        call ieee_set_flag(ieee_all, .true.)

!check if all flags for IEEE_USUAL are set to true
        call ieee_get_flag(ieee_usual, usual_flags)
        expected_usual_flags = (/.true., .true., .true. /)
        do k = 1, 3
          if (usual_flags(k) .neqv. expected_usual_flags(k)) then
            error stop 5
          endif
        enddo

!check if either IEEE_OVERFLOW, IEEE_DIVIDE_BY_ZERO, IEEE_INVALID
!from IEEE_ALL are set to true
        call ieee_get_flag(ieee_all, all_flags)
        do k = 1, 3
           if (all_flags(k) .neqv. .true. ) then
              error stop 6
           endif
        enddo

!...test IEEE_USUAL array named constants for false
!for .FALSE. values
        call ieee_set_flag(ieee_usual, .false.)
        call ieee_get_flag(ieee_usual, usual_flags)
        expected_usual_flags = (/.false., .false., .false./)
        do k = 1, 3
          if (usual_flags(k) .neqv. expected_usual_flags(k)) then
            error stop 7
          endif
        enddo

!check if either IEEE_OVERFLOW, IEEE_DIVIDE_BY_ZERO, IEEE_INVALID
!from IEEE_ALL are set to false
        call ieee_get_flag(ieee_all, all_flags)
        do k = 1, 3
           if (all_flags(k) .neqv. .false. ) then
              error stop 8
           endif
        enddo


!set IEEE_ALL to false
        call ieee_set_flag(ieee_all,.false.)

!...test each individual exception flag
!IEEE_OVERFLOW

        call ieee_set_flag(IEEE_OVERFLOW, .true.)
        call ieee_get_flag(ieee_all, all_flags)
        if (all_flags(1) .neqv. .true. ) then
           error stop 9
        endif

!IEEE_DIVIDE_BY_ZERO
        call ieee_set_flag(IEEE_DIVIDE_BY_ZERO, .true.)
        call ieee_get_flag(ieee_all, all_flags)
        if (all_flags(2) .neqv. .true. ) then
           error stop 10
        endif

!IEEE_INVALID
        call ieee_set_flag(IEEE_INVALID, .true.)
        call ieee_get_flag(ieee_all, all_flags)
        if (all_flags(3) .neqv. .true. ) then
           error stop 11
        endif

!IEEE_UNDERFLOW
        call ieee_set_flag(IEEE_UNDERFLOW, .true.)
        call ieee_get_flag(ieee_all, all_flags)
        if (all_flags(4) .neqv. .true. ) then
           error stop 12
        endif

!IEEE_INEXACT
        call ieee_set_flag(IEEE_INEXACT, .true.)
        call ieee_get_flag(ieee_all, all_flags)
        if (all_flags(5) .neqv. .true. ) then
           error stop 13
        endif

!check if either IEEE_OVERFLOW, IEEE_DIVIDE_BY_ZERO, IEEE_INVALID
!from IEEE_USUAL  array named constants are set to true

        call ieee_get_flag(ieee_usual, usual_flags)
        do k = 1, 3
           if ( usual_flags(k) .neqv. .true. ) then
              error stop 14
           endif
        enddo

!set each individual exception flag to false
!IEEE_OVERFLOW
        call ieee_set_flag(IEEE_OVERFLOW, .false.)
        call ieee_get_flag(ieee_all, all_flags)
        if (all_flags(1) .neqv. .false. ) then
           error stop 15
        endif
        call ieee_get_flag(ieee_usual, usual_flags)
        if (usual_flags(1) .neqv. .false. ) then
           error stop 16
        endif

!IEEE_DIVIDE_BY_ZERO
        call ieee_set_flag(IEEE_DIVIDE_BY_ZERO, .false.)
        call ieee_get_flag(ieee_all, all_flags)
        if (all_flags(2) .neqv. .false. ) then
           error stop 17
        endif
        call ieee_get_flag(ieee_usual, usual_flags)
        if (usual_flags(2) .neqv. .false. ) then
           error stop 18
        endif

!IEEE_INVALID
        call ieee_set_flag(IEEE_INVALID, .false.)
        call ieee_get_flag(ieee_all, all_flags)
        if (all_flags(3) .neqv. .false. ) then
           error stop 19
        endif
        call ieee_get_flag(ieee_usual, usual_flags)
        if (usual_flags(3) .neqv. .false. ) then
           error stop 20
        endif

!IEEE_UNDERFLOW
        call ieee_set_flag(IEEE_UNDERFLOW, .false.)
        call ieee_get_flag(ieee_all, all_flags)
        if (all_flags(4) .neqv. .false. ) then
           error stop 21
        endif

!IEEE_INEXACT
        call ieee_set_flag(IEEE_INEXACT, .false.)
        call ieee_get_flag(ieee_all, all_flags)
        if (all_flags(5) .neqv. .false. ) then
           error stop 22
        endif


        end
