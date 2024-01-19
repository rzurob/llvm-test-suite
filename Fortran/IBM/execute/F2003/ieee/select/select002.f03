! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 7, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SELECTED_REAL_KIND with different
!*                               numbers of arguments.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :This testcase tests the following scenarios:
!*
!* 1.Test with R available only
!* 2.Test with P available only
!* 3.Test with P available only
!* 4.Test both P and R unavailable
!* 5.Test the function with one available argument
!* 6.Test the function with one unavailable argument
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program selected_arg

        use ieee_arithmetic
        use constants_for_ieee

        logical :: flag_values(5)
        integer ::  i, zi, P, R

        ! ieee_selected_real_kind should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)

! Test with R available only

           P = 32
           do R = 0, 37
              zi = ieee_selected_real_kind(P, R)
              if (zi /= -1) error stop 1
                                 !"ieee_selected_real_kind  failed
           end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 400
        enddo

        call ieee_set_flag(ieee_all,.false.)


           P = 32
           do R = 38,307
              zi = ieee_selected_real_kind(P, R)
              if (zi /= -1) error stop 2
                                 !"ieee_selected_real_kind  failed
           end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 500
        enddo

        call ieee_set_flag(ieee_all,.false.)

! Test with P available only

           R = 308
           do P = 0,6
              zi = ieee_selected_real_kind(P, R)
              if (zi /= -2) error stop 3
                                 !"ieee_selected_real_kind  failed
           end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 600
        enddo

        call ieee_set_flag(ieee_all,.false.)

           R = 308
           do P = 7,15
              zi = ieee_selected_real_kind(P, R)
              if (zi /= -2) error stop 4
                                 !"ieee_selected_real_kind  failed
           end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 700
        enddo

        call ieee_set_flag(ieee_all,.false.)

           R = 308
           do P = 16,31
              zi = ieee_selected_real_kind(P, R)
              if (zi /= -2) error stop 5
                                 !"ieee_selected_real_kind  failed
           end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 800
        enddo

        call ieee_set_flag(ieee_all,.false.)


! Test both P and R unavailable

           R = 308
           P = 32
           zi = ieee_selected_real_kind(P, R)
           if (zi /= -3) error stop 6
                                 !"ieee_selected_real_kind  failed

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 900
        enddo

        call ieee_set_flag(ieee_all,.false.)

! Test the function with one available argument

           do P = 0,6
           zi = ieee_selected_real_kind(P)
           if (zi /= 4) error stop 7
                                 !"ieee_selected_real_kind  failed
	   end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 1000
        enddo

        call ieee_set_flag(ieee_all,.false.)

           do P = 7,15
           zi = ieee_selected_real_kind(P)
           if (zi /= 8) error stop 8
                                 !"ieee_selected_real_kind  failed
           end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 1100
        enddo

        call ieee_set_flag(ieee_all,.false.)

           do P = 16,31
           zi = ieee_selected_real_kind(P)
           if (zi /= 16) error stop 9
                                 !"ieee_selected_real_kind  failed
           end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 1200
        enddo

        call ieee_set_flag(ieee_all,.false.)

! Test the function with one unavailable argument

           P = 32
           zi = ieee_selected_real_kind(P)
           if (zi /= -1) error stop 10
                                 !"ieee_selected_real_kind  failed
        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 1300
        enddo

        call ieee_set_flag(ieee_all,.false.)

	    end program

