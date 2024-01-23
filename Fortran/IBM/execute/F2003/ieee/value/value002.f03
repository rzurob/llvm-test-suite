! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 12, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_VALUE with variables.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        include 'ieeeconsts.h'

	program value_val

        use ieee_arithmetic
        use constants_for_ieee

        real*4 :: a
        type(ieee_class_type) :: Y
        real*8 :: b
	real*16 :: c
        logical, dimension(5) :: flag_values
        integer :: i

!       ieee_value should not set any flags.  Clear all flags and
!       check at the end that all flags are clear.
        call ieee_set_flag(ieee_all,.false.)

!***********************
!       Test real*4
!***********************

	Y = ieee_signaling_nan

        a = ieee_value(a, Y)
        if (ieee_class(a) /= Y) error stop 1

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(100+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_quiet_nan

        a = ieee_value(a, Y)
        if (ieee_class(a) /= Y) error stop 2

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(110+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_inf

        a = ieee_value(a, Y)
        if (ieee_class(a) /= Y) error stop 3

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(120+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_inf

        a = ieee_value(a, Y)
        if (ieee_class(a) /= Y) error stop 4

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(130+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_normal

        a = ieee_value(a, Y)
        if (ieee_class(a) /= Y) error stop 5

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(140+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_normal

        a = ieee_value(a, Y)
        if (ieee_class(a) /= Y) error stop 6

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(150+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_denormal

        a = ieee_value(a, Y)
        if (ieee_class(a) /= Y) error stop 7

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_denormal

        a = ieee_value(a, Y)
        if (ieee_class(a) /= Y) error stop 8

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_zero

        a = ieee_value(a, Y)
        if (ieee_class(a) /= Y) error stop 9

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(170+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_zero

        a = ieee_value(a, Y)
        if (ieee_class(a) /= Y) error stop 10

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(180+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)



!***********************
!       Test real*8
!***********************


	Y = ieee_signaling_nan

        b = ieee_value(b, Y)
        if (ieee_class(b) /= Y) error stop 11

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(100+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_quiet_nan

        b = ieee_value(b, Y)
        if (ieee_class(b) /= Y) error stop 12

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(110+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_inf

        b = ieee_value(b, Y)
        if (ieee_class(b) /= Y) error stop 13

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(120+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_inf

        b = ieee_value(b, Y)
        if (ieee_class(b) /= Y) error stop 14

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(130+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_normal

        b = ieee_value(b, Y)
        if (ieee_class(b) /= Y) error stop 15

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(140+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_normal

        b = ieee_value(b, Y)
        if (ieee_class(b) /= Y) error stop 16

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(150+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_denormal

        b = ieee_value(b, Y)
        if (ieee_class(b) /= Y) error stop 17

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_denormal

        b = ieee_value(b, Y)
        if (ieee_class(b) /= Y) error stop 18

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_zero

        b = ieee_value(b, Y)
        if (ieee_class(b) /= Y) error stop 19

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(170+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_zero

        b = ieee_value(b, Y)
        if (ieee_class(b) /= Y) error stop 20

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(180+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)

!***********************
!       Test real*16
!***********************

	Y = ieee_signaling_nan

        c = ieee_value(c, Y)
        if (ieee_class(c) /= Y) error stop 21

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(100+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_quiet_nan

        c = ieee_value(c, Y)
        if (ieee_class(c) /= Y) error stop 22

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(110+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_inf

        c = ieee_value(c, Y)
        if (ieee_class(c) /= Y) error stop 23

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(120+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_inf

        c = ieee_value(c, Y)
        if (ieee_class(c) /= Y) error stop 24

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(130+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_normal

        c = ieee_value(c, Y)
        if (ieee_class(c) /= Y) error stop 25

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(140+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_normal

        c = ieee_value(c, Y)
        if (ieee_class(c) /= Y) error stop 26

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(150+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_denormal

        c = ieee_value(c, Y)
        if (ieee_class(c) /= Y) error stop 27

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_denormal

        c = ieee_value(c, Y)
        if (ieee_class(c) /= Y) error stop 28

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_negative_zero

        c = ieee_value(c, Y)
        if (ieee_class(c) /= Y) error stop 29

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(170+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        Y = ieee_positive_zero

        c = ieee_value(c, Y)
        if (ieee_class(c) /= Y) error stop 30

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(180+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        end program

