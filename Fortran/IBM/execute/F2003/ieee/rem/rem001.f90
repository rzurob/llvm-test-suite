! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh rem001
! %COMPOPTS: -qfree=f90 -qfloat=rrm:nofold -qieee=Near -qstrict
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.mod
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Alexandru Mihaileanu
!*  DATE                       : March 14, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_REM and rounding to nearest.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : This testcase tests if setting rounding
!*  mode to the nearest would affect the results of the function ieee_rem.
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       program fxieee11

        use ieee_arithmetic
        use ieee_exceptions
        use constants_for_ieee
  implicit none

        real*4 :: xr_4, yr_4, zr_4, a, res_4
        real*8 :: xr_8, yr_8, zr_8, a_8, res_8
        real*16 :: xr_16, yr_16, zr_16, a_16, res_16
        integer :: n, i, ires4
        integer*8 :: n8, ires8, ires16(2)
        logical precision_R8

        equivalence(ires4, res_4)
        equivalence(ires4, zr_4)
        equivalence(ires8, res_8)
        equivalence(ires8, zr_8)
        equivalence(ires16, res_16)
        equivalence(ires16, zr_16)

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

    real*16, parameter, dimension(4) :: xvalues_16 = &
     &       (/9.8_16**28, 1.9_16**302, -6.8_16**(-93), 3.9_16**(-89)/)
        real*16, parameter, dimension(4) :: yvalues_16 = &
     &       (/2.5_16**29, 3.4_16**(-10), 3.2_16**24, -1.3_16**(-16)/)
        real*16, dimension(4) :: results_16

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

        call ieee_set_rounding_mode(rt_nearest)
        call ieee_get_rounding_mode(rtype)
        if (rtype /= rt_nearest)  error stop 1

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

        ! ieee_rem is allowed to set flags like inexact and div_by_zero
        ! depending on input.
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(40+i)
        enddo

                do i = 1, 2
                   n = xvalues(i) / yvalues(i)
                   zr_4 = xvalues(i) - n *yvalues(i)
                   if (results(i) /= zr_4) error stop 4
                enddo

!**************************


        a_8 = 99.9
        res_8 = ieee_rint(a_8)
        if (res_8 /= 100.0) error stop 5

        call ieee_set_flag(ieee_all,.false.)

!       Test real*8
        xr_8 = 3.0_8
        yr_8 = 2.0_8
        zr_8 = ieee_rem(xr_8, yr_8)
        if (zr_8 /= -1.0 ) error stop 6

!       Test array of kind 8
        results_8 = ieee_rem(xvalues_8, yvalues_8)

        ! Now check that no flags were turned on.
        ! ieee_rem is allowed to set ieee_inexact depending on input
        call ieee_get_flag(ieee_all,flag_values)
        if (flag_values(1) .neqv. .false.)error stop 51
        if (flag_values(2) .neqv. .false.)error stop 52
        if (flag_values(3) .neqv. .true.)error stop 53
        if (flag_values(4) .neqv. .false.)error stop 54
        if (.not. precision_R8(results_8(1), 0.503116887977632490d34)) error stop 117
        if (.not. precision_R8(results_8(2), 57643227.7923734784d0)) error stop 118
        if (.not. precision_R8(results_8(3), -0.175301230959759361d-14)) error stop 119
        if (.not. precision_R8(results_8(4), 0.639987015502766506d-23)) error stop 120

!***************************

        a_16 = -123456.543_16
        res_16 = ieee_rint(a_16)
        if (res_16 /= -123457.0) error stop 8

        call ieee_set_flag(ieee_all, .false.)
!       test real*16
        xr_16 = 5.0_16
        yr_16 = 2.0_16
        zr_16 = ieee_rem(xr_16, yr_16)
        if (zr_16 /= 1.0 ) error stop 9
        call ieee_get_flag(ieee_all, flag_values)
        do i = 1,4
          if (flag_values(i) .neqv. .false.) call zzrc(150+i)
        enddo

!       Test array of kind 16

        results_16 = ieee_rem(xvalues_16, yvalues_16)
    do i = 1, 4
       zr_16 = xvalues_16(i) - yvalues_16(i) * nint &
     &                     (xvalues_16(i)/yvalues_16(i),8)
       if (results_16(i) /= zr_16) error stop 10
    enddo

!*****************************

! Test zero

!       Test real*4 /real*8 / real*16  for  n = -0, N = -0

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = -0.0
        yr_4 = 3.0
        xr_8 = -0.0_8
        yr_8 = 900.9_8
        xr_16 = -0.0_16
        yr_16 = 234.6_16

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iNZERO_4 .and. zr_4 /= 0.0) error stop 11

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iNZERO_8 .and. zr_8 /= 0.0) error stop 12

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(60+i)
        enddo


        zr_16 = ieee_rem(xr_16, yr_16)
        if (ires16(1) /= iPZERO_16(1).and. zr_16 /= 0.0) error stop 13

!       Test real*4 /real*8 / real*16  for n = -0, N = -0

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = 0.0
        yr_4 = -3.0
        xr_8 = 0.0_8
        yr_8 = -900.9_8
        xr_16 = 0.0_16
        yr_16 = -234.6_16

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iPZERO_4 .and. zr_4 /= 0.0) error stop 14

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iPZERO_4 .and. zr_8 /= 0.0) error stop 15

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(70+i)
        enddo


        zr_16 = ieee_rem(xr_16, yr_16)
        if (ires16(1) /= iPZERO_16(1).and. zr_16 /= 0.0) error stop 16


!       Test real*4 /real*8 / real*16  for n = -0, N = -0

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = 0.0
        yr_4 = -3.0
        xr_8 = 0.0_8
        yr_8 = -900.9_8
        xr_16 = 0.0_16
        yr_16 = -234.6_16

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iPZERO_4 .and. zr_4 /= 0.0) error stop 17

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iPZERO_8 .and. zr_8 /= 0.0) error stop 18

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(80+i)
        enddo


        zr_16 = ieee_rem(xr_16, yr_16)
        if (ires16(1) /= iPZERO_16(1) .and. zr_16 /= 0.0) error stop 19
        call ieee_get_status(status_value)


! Test Infinities


        call ieee_set_flag(ieee_all,.false.)

        xr_4 = PINF_4
        yr_4 = -2.0
        xr_8 = PINF_8
        yr_8 = 900.9_8
        xr_16 = PINF_16
        yr_16 = 234.6_16

        zr_4 = ieee_rem(xr_4, yr_4)
        if (zr_4 == zr_4) error stop 20

        zr_8 = ieee_rem(xr_8, yr_8)
        if (zr_8 == zr_8) error stop 21

        zr_16 = ieee_rem(xr_16, yr_16)
        if (zr_16 == zr_16) error stop 22

        ! Now check that no flags were turned on (except invalid).
        call ieee_get_flag(ieee_all,flag_values)
        if (flag_values(1) .neqv. .false.)error stop 91
        if (flag_values(2) .neqv. .false.)error stop 92
        if (flag_values(3) .neqv. .true.) error stop 93
        if (flag_values(4) .neqv. .false.)error stop 94

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = NINF_4
        yr_4 = -2.0
        xr_8 = NINF_8
        yr_8 = 900.9_8
        xr_16 = NINF_16
        yr_16 = 234.6_16

        zr_4 = ieee_rem(xr_4, yr_4)
        if (zr_4 == zr_4) error stop 23

        call ieee_set_flag(ieee_all,.false.)
        zr_8 = ieee_rem(xr_8, yr_8)
        if (zr_8 == zr_8) error stop 24

        zr_16 = ieee_rem(xr_16, yr_16)
        if (zr_16 == zr_16) error stop 25

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        if (flag_values(1) .neqv. .false.)error stop 101
        if (flag_values(2) .neqv. .false.)error stop 102
        if (flag_values(3) .neqv. .true.) error stop 103
        if (flag_values(4) .neqv. .false.)error stop 104
        if (flag_values(5) .neqv. .false.)error stop 105

! Test NANs

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = PNANQ_4
        yr_4 = -2.0
        xr_8 = PNANQ_8
        yr_8 = 900.9_8
        xr_16 = PNANQ_16
        yr_16 = 234.6_16

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iPNANQ_4) error stop 26

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iPNANQ_8) error stop 27

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        if (flag_values(1) .neqv. .false.)error stop 111
        if (flag_values(2) .neqv. .false.)error stop 112
        if (flag_values(3) .neqv. .false.)error stop 113
        if (flag_values(4) .neqv. .false.)error stop 114
        if (flag_values(5) .neqv. .false.)error stop 115


        zr_16 = ieee_rem(xr_16, yr_16)
        if (ires16(1) /= iPNANQ_16(1)) error stop 28

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = NNANQ_4
        yr_4 = -2.0
        xr_8 = NNANQ_8
        yr_8 = 900.9_8
        xr_16 = NNANQ_16
        yr_16 = 234.6_16

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iNNANQ_4) error stop 29

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iNNANQ_8) error stop 30

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        if (flag_values(1) .neqv. .false.)error stop 111
        if (flag_values(2) .neqv. .false.)error stop 112
        if (flag_values(3) .neqv. .false.)error stop 113
        if (flag_values(4) .neqv. .false.)error stop 114
        if (flag_values(5) .neqv. .false.)error stop 115

        zr_16 = ieee_rem(xr_16, yr_16)
        if (ires16(1) /= iNNANQ_16(1)) error stop 31

        call ieee_set_flag(ieee_all,.false.)

        xr_4 = PNANS_4
        yr_4 = -2.0
        xr_8 = PNANS_8
        yr_8 = 900.9_8
        xr_16 = PNANS_16
        yr_16 = 234.6_16

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iPNANQ_4) error stop 32

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (120+i)
        end do

        call ieee_set_flag(ieee_all,.false.)

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iPNANQ_8) error stop 33

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (125+i)
        end do


        call ieee_set_flag(ieee_all,.false.)

        zr_16 = ieee_rem(xr_16, yr_16)
        if (ires16(1) /= iPNANQ_16(1)) error stop 34

        call ieee_get_flag(ieee_all,flag_values)
        ! ieee_rem is allowed to set ieee_inexact for real*16
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (130+i)
        end do


        call ieee_set_flag(ieee_all,.false.)

        xr_4 = NNANS_4
        yr_4 = -2.0
        xr_8 = NNANS_8
        yr_8 = 900.9_8
        xr_16 = NNANS_16
        yr_16 = 234.6_16

        zr_4 = ieee_rem(xr_4, yr_4)
        if (ires4 /= iNNANQ_4) error stop 35

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (135+i)
        end do


        call ieee_set_flag(ieee_all,.false.)

        zr_8 = ieee_rem(xr_8, yr_8)
        if (ires8 /= iNNANQ_8) error stop 36

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (140+i)
        end do


        call ieee_set_flag(ieee_all,.false.)


        zr_16 = ieee_rem(xr_16, yr_16)
        if (ires16(1) /= iNNANQ_16(1)) error stop 37

        call ieee_get_flag(ieee_all,flag_values)
        ! ieee_rem is allowed to set ieee_inexact for real*16
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (145+i)
        end do


        end
