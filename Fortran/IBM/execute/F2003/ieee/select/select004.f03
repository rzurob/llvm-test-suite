! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 7, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SELECTED_REAL_KIND for special real
!*                               numbers when -qautodbl option is used.
!*                               None of the reals will be promoted.
!*  SECONDARY FUNCTIONS TESTED : -qautodbl
!*
!*  REQUIRED COMPILER OPTIONS  : -qautodbl
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : This testcase follows:
!*
!* 1.Testing real ZERO(+/-)
!* 2.Testing INFINITIES(+/-)
!* 3.Testing Denormals(+/-)
!* 4.Testing NANQ/NANS(+/-)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program selected_reals

        use ieee_arithmetic
        use constants_for_ieee

	logical :: flag_values(5)
        integer :: i,xi, yi, zi
        real*4 :: a
        real*8 :: b
        real*16 :: c

        ! ieee_selected_real_kind should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)

! Testing ZERO

        a = PZERO_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 1

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 101
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = PZERO_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 2

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 102
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = PZERO_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 3

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 103
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = NZERO_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 4

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 104
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = NZERO_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 5

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 105
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = NZERO_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 6

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 106
        enddo

        call ieee_set_flag(ieee_all,.false.)

! Testing INFINITIES

        a = PINF_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 7

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 107
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = PINF_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 8

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 108
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = PINF_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 9

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 109
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = NINF_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 10

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 110
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = NINF_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 11

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 111
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = NINF_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 12

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 112
        enddo

        call ieee_set_flag(ieee_all,.false.)

! Testing denormals

        a = PHD_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 13

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 113
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = PHD_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 14

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 114
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = PHD_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 15

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 115
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = NHD_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 16

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 116
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = NHD_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 17

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 117
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = NHD_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 18

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 118
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = PTD_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 19

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 119
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = PTD_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 20

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 120
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = PTD_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 21

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 121
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = NTD_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 22

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 122
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = NTD_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 23

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 123
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = NTD_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 24

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 124
        enddo

        call ieee_set_flag(ieee_all,.false.)

! Testing NANs

        a = PNANQ_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 25

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 125
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = PNANQ_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 26

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 126
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = PNANQ_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 27

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 127
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = NNANQ_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 28

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 128
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = NNANQ_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 29

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 129
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = NNANQ_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 30

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 130
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = PNANS_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 31

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 131
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = PNANS_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 32

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 132
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = PNANS_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 33

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 133
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = NNANS_4
        xi = precision(a)
        yi = range(a)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 4) error stop 34

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 134
        enddo

        call ieee_set_flag(ieee_all,.false.)

        b = NNANS_8
        xi = precision(b)
        yi = range(b)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 8) error stop 35

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 135
        enddo

        call ieee_set_flag(ieee_all,.false.)

        c = NNANS_16
        xi = precision(c)
        yi = range(c)
        zi = ieee_selected_real_kind(xi, yi)
        if (zi /= 16) error stop 36

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 136
        enddo

        call ieee_set_flag(ieee_all,.false.)

	end program

