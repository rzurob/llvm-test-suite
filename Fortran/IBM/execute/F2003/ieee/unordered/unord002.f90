! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh unord002
! %COMPOPTS: -qfree=f90 -qstrict
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 8, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_UNORDERED NANs with constants.
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
    program fxieee21
        use ieee_arithmetic
        use constants_for_ieee

	implicit none

        real*4 :: a, b
        real*8 :: c, d
        real*16 :: e, f
        real*4, parameter :: val1 = PNANQ_4
        real*4, parameter :: val2 = PNANS_4
        real*4, parameter :: val3 = NNANQ_4
        real*4, parameter :: val4 = NNANS_4

        real*8, parameter :: val1_8 = PNANQ_8
        real*8, parameter :: val2_8 = PNANS_8
        real*8, parameter :: val3_8 = NNANQ_8
        real*8, parameter :: val4_8 = NNANS_8

        real*16, parameter :: val1_16 = PNANQ_16
        real*16, parameter :: val2_16 = PNANS_16
        real*16, parameter :: val3_16 = NNANQ_16
        real*16, parameter :: val4_16 = NNANS_16

        logical :: res, flag_values(5)


        integer :: i

        ! ieee_unordered should not set any flags.  Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)
!*****************************************************************************
!  Test real*4
!*****************************************************************************

        a = PZERO_4
        b = NZERO_4
        res = ieee_unordered(val1, a)
        if ( res .eqv. .false.) error stop 1

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(10+i)
              enddo

        res = ieee_unordered(val1, b)
        if ( res .eqv. .false.) error stop 2

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(20+i)
              enddo
!*****************

        a = PINF_4
        b = NINF_4
        res = ieee_unordered(val2, a)
        if ( res .eqv. .false.) error stop 3

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(40+i)
              enddo

        res = ieee_unordered(val2, b)
        if ( res .eqv. .false.) error stop 4

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(50+i)
              enddo
!*****************

        a = PHD_4
        b = NHD_4
        res = ieee_unordered(val3, a)
        if ( res .eqv. .false.) error stop 5

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(60+i)
              enddo

        res = ieee_unordered(val3, b)
        if ( res .eqv. .false.) error stop 6

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(70+i)
              enddo
!*****************

        a = PTD_4
        b = NTD_4
        res = ieee_unordered(val4, a)
        if ( res .eqv. .false.) error stop 7

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(80+i)
              enddo

        res = ieee_unordered(val4, b)
        if ( res .eqv. .false.) error stop 8

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(90+i)
              enddo

!*****************************************************************************
!  Test real*8
!*****************************************************************************

        c = PZERO_8
        d = NZERO_8
        res = ieee_unordered(val1_8, c)
        if ( res .eqv. .false.) error stop 9

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(100+i)
              enddo

        res = ieee_unordered(val1_8, d)
        if ( res .eqv. .false.) error stop 10

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(110+i)
              enddo
!*****************

        a = PINF_8
        b = NINF_8
        res = ieee_unordered(val2_8, c)
        if ( res .eqv. .false.) error stop 11

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(120+i)
              enddo

        res = ieee_unordered(val2_8, d)
        if ( res .eqv. .false.) error stop 12

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(130+i)
              enddo
!*****************

        a = PHD_8
        b = NHD_8
        res = ieee_unordered(val3_8, c)
        if ( res .eqv. .false.) error stop 13

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(140+i)
              enddo

        res = ieee_unordered(val3_8, d)
        if ( res .eqv. .false.) error stop 14

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(150+i)
              enddo
!*****************

        a = PTD_8
        b = NTD_8
        res = ieee_unordered(val4_8, c)
        if ( res .eqv. .false.) error stop 15

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(160+i)
              enddo

        res = ieee_unordered(val4_8, d)
        if ( res .eqv. .false.) error stop 16

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(170+i)
              enddo

!*****************************************************************************
!  Test real*16
!*****************************************************************************

        e = PZERO_16
        res = ieee_unordered(val1_16, e)
        if ( res .eqv. .false.) error stop 17

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(180+i)
              enddo

!*****************

        a = PINF_16
        b = NINF_16
        res = ieee_unordered(val2_16, e)
        if ( res .eqv. .false.) error stop 18

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(190+i)
              enddo

        res = ieee_unordered(val2_16, f)
        if ( res .eqv. .false.) error stop 19

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(200+i)
              enddo

        end program
