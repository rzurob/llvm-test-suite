! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_REM and rounding to -INF.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : This testcase tests if setting rounding
!*  mode to the -INF would affect the results of the function ieee_rem.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       program fxieee11

        use ieee_arithmetic
        use ieee_exceptions
        use constants_for_ieee
  implicit none

        real*4 :: xr_4, yr_4, zr_4, a, res_4
        real*8 :: xr_8, yr_8, zr_8, a_8, res_8
  integer :: n, i, ires4
  integer*8 :: n8, ires8

        equivalence(ires4, res_4)
        equivalence(ires4, zr_4)
        equivalence(ires8, res_8)
        equivalence(ires8, zr_8)

        real*4, parameter, dimension(4) :: xvalues = &
     &       (/1.2**2, 2.5**3, 3.6**(-8), -6.8**(-9)/)
        real*4, dimension(4) :: yvalues = &
     &       (/6.7**3, -7.6**(-2), 8.1**2, 4.7**(-7)/)
        real*4, dimension(4) :: results

    real*8, parameter, dimension(4) :: xvalues_8 = &
     &       (/9.8_8**34, 1.9_8**29, -2.8_8**(-33), 5.6_8**(-31)/)
        real*8, parameter, dimension(4) :: yvalues_8 = &
     &       (/2.5_8**26, 3.4_8**(-21), 8.2_8**23, -5.6_8**(-27)/)
        real*8, dimension(4) :: results_8

        logical, dimension(5) :: flag_values, expect_value
         type(ieee_round_type) :: rtype
         type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
         type(ieee_round_type), parameter :: rt_20 = IEEE_TO_ZERO
         type(ieee_round_type), parameter :: rt_up = IEEE_UP
         type(ieee_round_type), parameter :: rt_down = IEEE_DOWN
         type(ieee_round_type), parameter :: rt_other = IEEE_OTHER
         type(ieee_status_type) :: status_value


!  test rounding to nearest followed by ieee_rem

        call ieee_set_flag(ieee_all,.false.)

         call ieee_set_rounding_mode(rt_down)
         call ieee_get_rounding_mode(rtype)
         if (rtype /= rt_down)  error stop 1

        a = 4.3
        res_4 = ieee_rint(a)
        if (res_4 /= 4.0) error stop 2

!       Test real*4
        xr_4 = 4.0
        yr_4 = 3.0
        zr_4 = ieee_rem(xr_4, yr_4)
        if (zr_4 /= 1.0 ) error stop 3

        call ieee_set_flag(ieee_all,.false.)
!       Test array of kind 4
        results = ieee_rem(xvalues, yvalues)
        call ieee_get_flag(ieee_all,flag_values)

        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(40+i)
        enddo

                do i = 1, 2
                   n = xvalues(i) / yvalues(i)
                   zr_4 = xvalues(i) - n *yvalues(i)
                   if (results(i) /= zr_4) error stop 4
                enddo

!**************************


        call ieee_set_flag(ieee_all,.false.)

        a_8 = 99.9
        res_8 = ieee_rint(a_8)
        if (res_8 /= 99.0) error stop 5

!       Test real*8
        xr_8 = 3.0_8
        yr_8 = 2.0_8
        zr_8 = ieee_rem(xr_8, yr_8)
        if (zr_8 /= -1.0 ) error stop 6

!       Test array of kind 8
!       results for first two elements are undefined, but ieee_invalid should signal.
        results_8 = ieee_rem(xvalues_8, yvalues_8)

        ! Now check that no flags were turned on.
        expect_value = (/.false.,.false.,.true.,.false.,.false./)
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. expect_value(i))call zzrc(50+i)
        enddo

                do i = 3, 4
                   n8 = xvalues_8(i) / yvalues_8(i)
                   zr_8 = xvalues_8(i) - n8 *yvalues_8(i)
                   if (results_8(i) /= zr_8) error stop 7
                enddo

!***************************

! Test zero

!       Test real*4 /real*8   for  n = -0, N = -0

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = -0.0
        yr_4 = 3.0
        xr_8 = -0.0_8
        yr_8 = 900.9_8

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iNZERO_4 .and. zr_4 /= 0.0) error stop 11

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iNZERO_8 .and. zr_8 /= 0.0) error stop 12

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(60+i)
        enddo


!       Test real*4 /real*8  for n = -0, N = -0

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = 0.0
        yr_4 = -3.0
        xr_8 = 0.0_8
        yr_8 = -900.9_8

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iPZERO_4 .and. zr_4 /= 0.0) error stop 14

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iPZERO_4 .and. zr_8 /= 0.0) error stop 15

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(70+i)
        enddo


!       Test real*4 /real*8 for n = -0, N = -0

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = 0.0
        yr_4 = -3.0
        xr_8 = 0.0_8
        yr_8 = -900.9_8

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iPZERO_4 .and. zr_4 /= 0.0) error stop 17

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iPZERO_8 .and. zr_8 /= 0.0) error stop 18

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(65+i)
        enddo


   call ieee_get_status(status_value)

! Test Infinities


        call ieee_set_flag(ieee_all,.false.)

        xr_4 = PINF_4
        yr_4 = -2.0
        xr_8 = PINF_8
        yr_8 = 900.9_8

        zr_4 = ieee_rem(xr_4, yr_4)
        if (zr_4 == zr_4) error stop 20

        zr_8 = ieee_rem(xr_8, yr_8)
        if (zr_8 == zr_8) error stop 21

        ! Now check that no flags were turned on.
        expect_value = (/.false.,.false.,.true.,.false.,.false./)
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. expect_value(i))call zzrc(80+i)
        enddo


        call ieee_set_flag(ieee_all,.false.)

        xr_4 = NINF_4
        yr_4 = -2.0
        xr_8 = NINF_8
        yr_8 = 900.9_8

        zr_4 = ieee_rem(xr_4, yr_4)
        if (zr_4 == zr_4) error stop 23

        call ieee_set_flag(ieee_all,.false.)
        zr_8 = ieee_rem(xr_8, yr_8)
        if (zr_8 == zr_8) error stop 24

        ! Now check that no flags were turned on.
        expect_value = (/.false.,.false.,.true.,.false.,.false./)
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. expect_value(i))call zzrc(90+i)
        enddo


! Test NANs

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = PNANQ_4
        yr_4 = -2.0
        xr_8 = PNANQ_8
        yr_8 = 900.9_8

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iPNANQ_4) error stop 26

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iPNANQ_8) error stop 27

        ! Now check that no flags were turned on.
        expect_value = (/.false.,.false.,.false.,.false.,.false./)
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. expect_value(i))call zzrc(100+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = NNANQ_4
        yr_4 = -2.0
        xr_8 = NNANQ_8
        yr_8 = 900.9_8

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iNNANQ_4) error stop 29

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iNNANQ_8) error stop 30

        ! Now check that no flags were turned on.
        expect_value = (/.false.,.false.,.false.,.false.,.false./)
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. expect_value(i))call zzrc(110+i)
        enddo


        call ieee_set_flag(ieee_all,.false.)

        xr_4 = PNANS_4
        yr_4 = -2.0
        xr_8 = PNANS_8
        yr_8 = 900.9_8

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iPNANQ_4) error stop 32

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (115+i)
        end do

        call ieee_set_flag(ieee_all,.false.)

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iPNANQ_8) error stop 33

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (120+i)
        end do


        call ieee_set_flag(ieee_all,.false.)

        xr_4 = NNANS_4
        yr_4 = -2.0
        xr_8 = NNANS_8
        yr_8 = 900.9_8

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iNNANQ_4) error stop 35

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (130+i)
        end do


        call ieee_set_flag(ieee_all,.false.)

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iNNANQ_8) error stop 36

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (135+i)
        end do


        end