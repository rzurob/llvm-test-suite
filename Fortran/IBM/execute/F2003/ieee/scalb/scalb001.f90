! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SCALB with reals.
!*                               IEEE_SCALB = 2**I * X
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : This testcase has the following scenarios:
!*
!* 1.Test positive real*4/real*8/real*16 when I = 0
!* 2.Test negative real*4/real*8/real*16 when I = 0
!* 3.Test positive real*4/real*8/real*16 when I = -0
!* 4.Test negative real*4/real*8/real*16 when I = -0
!* 5.Test real argument = +INF
!* 6.Test real argument = -INF
!* 7.Test a large +I - OVERFLOW
!* 8.Test UNDERFLOW
!* 9.Test argument = PZERO
!* 10.Test argument = NZERO
!* 11.Test argument = Positive Denormals
!* 12.Test argument = Negative Denormals
!* 13.Test argument = NANQ
!* 14.Test argument = NANS
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program scalb_reals

        use ieee_arithmetic
        use constants_for_ieee
	implicit none

        real*4 :: xr4, result4
        integer :: yi, i
	real*8 :: xr8, result8
        real*16 :: xr16, result16
        integer*4 :: iresult4
        integer*8 :: iresult8
        integer*8 :: iresult16(2)

        logical :: flag_values(5)
        logical :: flag_value
        type(ieee_status_type) :: status_value

        equivalence(iresult4, result4)
        equivalence(iresult8, result8)
        equivalence(iresult16, result16)


	call ieee_get_status(status_value)

!       Test positive real*4 when yi = 0

        xr4 = 4.0
        yi = 0
        result4 = ieee_scalb(xr4, yi)
        if (result4 /= xr4) error stop 1

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 201
        enddo


!       Test positive real*8 when I = 0

	call ieee_set_status(status_value)

        xr8 = 2.0_8
        yi = 0
        result8 = ieee_scalb(xr8, yi)
        if (result8 /= xr8 ) error stop 2

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 202
        enddo


!       Test positive real*16 when I = 0

	call ieee_set_status(status_value)

        xr16 = 8.0_16
        yi = 0
        result16 = ieee_scalb(xr16, yi)
        if (result16 /= xr16 ) error stop 3

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 203
        enddo


!       Test negative real*4 when I = 0

	call ieee_set_status(status_value)

        xr4 = -4.0
        yi = 0
        result4 = ieee_scalb(xr4, yi)
        if (result4 /= xr4 ) error stop 4

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 204
        enddo


!       Test negative real*8 when I = 0

	call ieee_set_status(status_value)

        xr8 = -2.0_8
        yi = 0
        result8 = ieee_scalb(xr8, yi)
        if (result8 /= xr8 ) error stop 5

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 205
        enddo


!       Test negative real*16 when I = 0

	call ieee_set_status(status_value)

        xr16 = -8.0_16
        yi = 0
        result16 = ieee_scalb(xr16, yi)
        if (result16 /= xr16 ) error stop 6

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 206
        enddo

!       Test positive real*4 when I = -0

	call ieee_set_status(status_value)

        xr4 = 4.0
        yi = -0
        result4 = ieee_scalb(xr4, yi)
        if (result4 /= xr4 ) error stop 7

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 207
        enddo


!       Test positive real*8 when yi = -0

	call ieee_set_status(status_value)

        xr8 = 2.0_8
        yi = -0
        result8 = ieee_scalb(xr8, yi)
        if (result8 /= xr8 ) error stop 8

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 208
        enddo


!       Test positive real*16 when I = -0

	call ieee_set_status(status_value)

        xr16 = 8.0_16
        yi = -0
        result16 = ieee_scalb(xr16, yi)
        if (result16 /= xr16 ) error stop 9

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 209
        enddo


!       Test negative real*4 when I = -0

	call ieee_set_status(status_value)

        xr4 = -4.0
        yi = -0
        result4 = ieee_scalb(xr4, yi)
        if (result4 /= xr4 ) error stop 10

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 210
        enddo


!       Test negative real*8 when I = -0

	call ieee_set_status(status_value)

        xr8 = -2.0_8
        yi = -0
        result8 = ieee_scalb(xr8, yi)
        if (result8 /= xr8 ) error stop 11

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 211
        enddo


!       Test negative real*16 when I = -0

	call ieee_set_status(status_value)

        xr16 = -8.0_16
        yi = -0
        result16 = ieee_scalb(xr16, yi)
        if (result16 /= xr16 ) error stop 12

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 212
        enddo



!       Test real argument = +INF

	call ieee_set_status(status_value)

        xr4 = X"7F800000"
        xr8 = X"7FF0000000000000"
        xr16 = X"7FF00000000000000000000000000000"
        yi = 56
        result4 = ieee_scalb(xr4, yi)
	if (result4 /= X"7F800000") error stop 13

        call ieee_get_flag(IEEE_OVERFLOW, flag_value)
	if (flag_value .eqv. .true.) error stop 14
				           !IEEE_OVERFLOW signaling for PINF!

	call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (result8 /= X"7FF0000000000000") error stop 15

        call ieee_get_flag(IEEE_OVERFLOW, flag_value)
        if (flag_value .eqv. .true.) error stop 16
                                           !IEEE_OVERFLOW signaling for PINF!

	call ieee_set_status(status_value)

        result16 = ieee_scalb(xr16, yi)
        if (result16 /= X"7FF00000000000000000000000000000") error stop 17

        call ieee_get_flag(IEEE_OVERFLOW, flag_value)
        if (flag_value .eqv. .true.) error stop 18
                                           !IEEE_OVERFLOW signaling for PINF!


	call ieee_set_status(status_value)

!       Test real argument = -INF

        xr4 = X"FF800000"
        xr8 = X"FFF0000000000000"
        xr16 = X"FFF00000000000000000000000000000"
        yi = 56
        result4 = ieee_scalb(xr4, yi)
        if (result4 /= X"FF800000") error stop 19

        call ieee_get_flag(IEEE_OVERFLOW, flag_value)
        if (flag_value .eqv. .true.) error stop 20
                                           !IEEE_OVERFLOW signaling for PINF!

	call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (result8 /= X"FFF0000000000000") error stop 21

        call ieee_get_flag(IEEE_OVERFLOW, flag_value)
        if (flag_value .eqv. .true.) error stop 22
                                           !IEEE_OVERFLOW signaling for PINF!

	call ieee_set_status(status_value)

        result16 = ieee_scalb(xr16, yi)
        if (result16 /= X"FFF00000000000000000000000000000") error stop 23

        call ieee_get_flag(IEEE_OVERFLOW, flag_value)
        if (flag_value .eqv. .true.) error stop 24
                                           !IEEE_OVERFLOW signaling for PINF!

	call ieee_set_status(status_value)


!Test a large +I

        yi = 2147483647
	xr4 = 16
	result4 = ieee_scalb(xr4, yi)
	if (result4 /= z"7f800000") error stop 25
				! "ieee_scalb failed for a large I

	call ieee_get_flag(IEEE_OVERFLOW, flag_value)
	if (flag_value .eqv. .false.) error stop 26
			          !IEEE_OVERFLOW not signaling for a large I!

	call ieee_set_status(status_value)

        yi = 2147483647
        xr8 = 16_8
        result8 = ieee_scalb(xr8, yi)
        if (result8 /= z"7ff0000000000000") error stop 27
                                    !ieee_scalb failed for large +i."

        call ieee_get_flag(IEEE_OVERFLOW, flag_value)
        if (flag_value .eqv. .false.) error stop 28
                                   !IEEE_OVERFLOW not signaling for large I!

!Test Underflow


	call ieee_set_status(status_value)

        yi = -9
        xr4 = 2.0_4**(-143)
        result4 = ieee_scalb(xr4, yi)
        if (result4 /= z"00000000") error stop 29
                                    !ieee_scalb failed

        call ieee_get_flag(IEEE_UNDERFLOW, flag_value)
        if (flag_value .eqv. .false.) error stop 30
                                    !IEEE_UNDERFLOW not signaling for a large I


	call ieee_set_status(status_value)

        xr4 = tiny(1.0)
        yi  = -24
        result4 = ieee_scalb(xr4, yi)
	xr4 = 2.0_4**yi*xr4
	if (result4 /= xr4) error stop 31

        call ieee_get_flag(IEEE_UNDERFLOW, flag_value)
        if (flag_value .eqv. .false.) error stop 32
                                                !IEEE_UNDERFLOW not signaling!

        call ieee_set_status(status_value)

! Test argument = PZERO

        xr4 = PZERO_4
        xr8 = PZERO_8
        xr16 = PZERO_16
        yi = 28

        result4 = ieee_scalb(xr4, yi)
        if (result4 /= PZERO_4) error stop 33

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 214
        enddo

        call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (result8 /= PZERO_8) error stop 34

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 215
        enddo

        call ieee_set_status(status_value)

        result16 = ieee_scalb(xr16, yi)
        if (result16 /= PZERO_16) error stop 35

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 216
        enddo


! Test argument = NZERO

        xr4 = NZERO_4
        xr8 = NZERO_8
        yi = 28

        result4 = ieee_scalb(xr4, yi)
        if (result4 /= NZERO_4) error stop 36

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 217
        enddo

        call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (result8 /= NZERO_8) error stop 37

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 218
        enddo

! Test argument = Positive Denormals

        xr4 = PHD_4
        xr8 = PHD_8
        yi = 0

        result4 = ieee_scalb(xr4, yi)
        if (result4 /= PHD_4) error stop 39

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 220
        enddo

        call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (result8 /= PHD_8) error stop 40

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 221
        enddo


        call ieee_set_status(status_value)

        xr4 = PTD_4
        xr8 = PTD_8
        yi = 0

        result4 = ieee_scalb(xr4, yi)
        if (result4 /= PTD_4) error stop 42

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 223
        enddo

        call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (result8 /= PTD_8) error stop 43

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 224
        enddo


! Test argument = -Denormals

        call ieee_set_status(status_value)

        xr4 = NHD_4
        xr8 = NHD_8
        yi = 0

        result4 = ieee_scalb(xr4, yi)
        if (result4 /= NHD_4) error stop 45

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 226
        enddo

        call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (result8 /= NHD_8) error stop 46

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 227
        enddo


        call ieee_set_status(status_value)

        xr4 = NTD_4
        xr8 = NTD_8
        yi = 0

        result4 = ieee_scalb(xr4, yi)
        if (result4 /= NTD_4) error stop 48

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 229
        enddo

        call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (result8 /= NTD_8) error stop 49

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 230
        enddo

        call ieee_set_status(status_value)

! Test argument = NANQ

        xr4 = PNANQ_4
        xr8 = PNANQ_8
        xr16 = PNANQ_16
        yi = 0

        result4 = ieee_scalb(xr4, yi)
        if (iresult4 /= iPNANQ_4) error stop 51

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 232
        enddo

        call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (iresult8 /= iPNANQ_8) error stop 52

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 233
        enddo

        call ieee_set_status(status_value)

        result16 = ieee_scalb(xr16, yi)
        if (iresult16(1) /= iPNANQ_16(1)) call zzrc(i+300)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 234
        enddo

        call ieee_set_status(status_value)

        xr4 = NNANQ_4
        xr8 = NNANQ_8
        xr16 = NNANQ_16
        yi = 0

        result4 = ieee_scalb(xr4, yi)
        if (iresult4 /= iNNANQ_4) error stop 53

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 235
        enddo

        call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (iresult8 /= iNNANQ_8) error stop 54

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 236
        enddo

        call ieee_set_status(status_value)

        result16 = ieee_scalb(xr16, yi)
        if (iresult16(1) /= iNNANQ_16(1)) call zzrc(i+400)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 237
        enddo

! Test argument = NANS

        call ieee_set_status(status_value)

        xr4 = PNANS_4
        xr8 = PNANS_8
        xr16 = PNANS_16
        yi = 0

        result4 = ieee_scalb(xr4, yi)
        if (iresult4 /= iPNANQ_4) error stop 55

        ! Now check that ieee_invalid flag is set
        call ieee_get_flag(IEEE_INVALID,flag_value)
            if (flag_value .eqv. .false.)error stop 238
                               !IEEE_INVALID did not signal NANS

        call ieee_set_status(status_value)

        result8 = ieee_scalb(xr8, yi)
        if (iresult8 /= iPNANQ_8) error stop 56

        ! Now check that ieee_invalid flag is set
        call ieee_get_flag(IEEE_INVALID,flag_value)
            if (flag_value .eqv. .false.)error stop 239
                               !IEEE_INVALID did not signal NANS


        call ieee_set_status(status_value)

        result16 = ieee_scalb(xr16, yi)
        if (iresult16(1) /= iPNANQ_16(1)) call zzrc(i+500)

        ! Now check that ieee_invalid flag is set
        call ieee_get_flag(IEEE_INVALID,flag_value)
            if (flag_value .eqv. .false.)error stop 240
                               !IEEE_INVALID did not signal NANS

        end program

