! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh unord003
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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Alexandru Mihaileanu
!*  DATE                       : March 8, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_UNORDERED NANs with variables.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
    program fxieee21

        use ieee_arithmetic
        use constants_for_ieee

	implicit none

        real*4 :: a, b
        real*8 :: c, d
        real*16 :: e, f
        real*4 :: val1 
        real*4 :: val2 
        real*4 :: val3 
        real*4 :: val4 

        real*8 :: val1_8 
        real*8 :: val2_8 
        real*8 :: val3_8 
        real*8 :: val4_8 

        real*16 :: val1_16 
        real*16 :: val2_16 
        real*16 :: val3_16 
        real*16 :: val4_16 
 
        logical :: res, flag_values(5)


        integer :: i

        val1 = PNANQ_4
        val2 = PNANS_4
        val3 = NNANQ_4
        val4 = NNANS_4

        val1_8 = PNANQ_8
        val2_8 = PNANS_8
        val3_8 = NNANQ_8
        val4_8 = NNANS_8

        val1_16 = PNANQ_16
        val2_16 = PNANS_16
        val3_16 = NNANQ_16
        val4_16 = NNANS_16
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
                                                !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(120+i)
              enddo

        call ieee_get_flag(ieee_all,flag_values)

        res = ieee_unordered(val2_8, d)

        if ( res .eqv. .false.) error stop 12
                                                !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(130+i)
              enddo
!*****************

        call ieee_get_flag(ieee_all,flag_values)
        a = PHD_8
        b = NHD_8
        res = ieee_unordered(val3_8, c)

        if ( res .eqv. .false.) error stop 13
                                                !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(140+i)
              enddo

        call ieee_get_flag(ieee_all,flag_values)

        res = ieee_unordered(val3_8, d)

        if ( res .eqv. .false.) error stop 14
                                                !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(150+i)
              enddo
!*****************

        call ieee_get_flag(ieee_all,flag_values)
        a = PTD_8
        b = NTD_8
        res = ieee_unordered(val4_8, c)

        if ( res .eqv. .false.) error stop 15
                                                !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(160+i)
              enddo

        call ieee_get_flag(ieee_all,flag_values)

        res = ieee_unordered(val4_8, d)

        if ( res .eqv. .false.) error stop 16
                                                !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(170+i)
              enddo

!*****************************************************************************
!  Test real*16
!*****************************************************************************

        call ieee_get_flag(ieee_all,flag_values)
        e = PZERO_16
        res = ieee_unordered(val1_16, e)

        if ( res .eqv. .false.) error stop 17
               			        	!ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(180+i)
              enddo

!*****************

        call ieee_get_flag(ieee_all,flag_values)
        a = PINF_16
        b = NINF_16
        res = ieee_unordered(val2_16, e)

        if ( res .eqv. .false.) error stop 18
                                                !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(190+i)
              enddo

        call ieee_get_flag(ieee_all,flag_values)

        res = ieee_unordered(val2_16, f)

        if ( res .eqv. .false.) error stop 19
                                                !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(200+i)
              enddo

        end program
