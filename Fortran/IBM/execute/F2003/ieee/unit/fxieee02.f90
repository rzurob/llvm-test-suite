! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee02
! %COMPOPTS: -qfree=f90
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
!*  DATE                       : February 5, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_COPY_SIGN
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
      program fxieee02

        use ieee_arithmetic
        use constants_for_ieee

        real*4 :: xr_4, yr_4
        real*8 :: xr_8, yr_8
        real*16 :: xr_16, yr_16
        real*4, parameter, dimension(16) :: values = &
     &       (/             &
     &       PNANS_4,       & ! Signaling NaN
     &       NNANS_4,       & ! Signaling NaN
     &       PNANQ_4,       & ! Quiet NaN
     &       NNANQ_4,       & ! Quiet NaN
     &       PINF_4,        & ! Positive INF
     &       huge(PINF_4),  & ! Positive Normal
     &       tiny(PINF_4),  & ! Positive Normal
     &       PHD_4,         & ! Positive Denormal
     &       PTD_4,         & ! Positive Denormal
     &       PZERO_4,       & ! Positive Zero
     &       NZERO_4,       & ! Negative Zero
     &       NTD_4,         & ! Negative Denormal
     &       NHD_4,         & ! Negative Denormal
     &       -tiny(PINF_4), & ! Negative Normal
     &       -huge(PINF_4), & ! Negative Normal
     &       NINF_4         & ! Negative INF
     &       /)

        real(4), dimension(16) :: xresults, ysigns
        integer*4, dimension(16) :: iresults

    real*8, parameter, dimension(16) :: values_8 = &
     &       (/             &
     &       PNANS_8,       & ! Signaling NaN
     &       NNANS_8,       & ! Signaling NaN
     &       PNANQ_8,       & ! Quiet NaN
     &       NNANQ_8,       & ! Quiet NaN
     &       PINF_8,        & ! Positive INF
     &       huge(PINF_8),  & ! Positive Normal
     &       tiny(PINF_8),  & ! Positive Normal
     &       PHD_8,         & ! Positive Denormal
     &       PTD_8,         & ! Positive Denormal
     &       PZERO_8,       & ! Positive Zero
     &       NZERO_8,       & ! Negative Zero
     &       NTD_8,         & ! Negative Denormal
     &       NHD_8,         & ! Negative Denormal
     &       -tiny(PINF_8), & ! Negative Normal
     &       -huge(PINF_8), & ! Negative Normal
     &       NINF_8         & ! Negative INF
     &       /)

        real*8, dimension(16) :: xresults_8, ysigns_8
        integer*8, dimension(16) :: iresults_8

        real*16, parameter, dimension(10) :: values_16 = &
     &       (/             &
     &       PNANS_16,       & ! Signaling NaN
     &       NNANS_16,       & ! Signaling NaN
     &       PNANQ_16,       & ! Quiet NaN
     &       NNANQ_16,       & ! Quiet NaN
     &       PINF_16,        & ! Positive INF
     &       huge(PINF_16),  & ! Positive Normal
     &       tiny(PINF_16),  & ! Positive Normal
     &       -tiny(PINF_16), & ! Negative Normal
     &       -huge(PINF_16), & ! Negative Normal
     &       NINF_16         & ! Negative INF
     &       /)

        real*16, dimension(10) :: xresults_16, ysigns_16
    integer*8 :: apns(2), anns(2), apnq(2), annq(2)

        logical, dimension(5) :: flag_values, original(5)
        integer :: i

        equivalence (iresults, xresults)
        equivalence (iresults_8, xresults_8)
    equivalence (apns, xresults_16(1))
    equivalence (anns, xresults_16(2))
    equivalence (apnq, xresults_16(3))
        equivalence (annq, xresults_16(4))

        call ieee_get_flag(ieee_all, original)
    flag_values = .false.
    call ieee_set_flag(IEEE_ALL, flag_values)
    flag_values = .true.
        !call ieee_get_flag(IEEE_ALL, flag_values)
      ! do i = 1, 5
       !   if (flag_values(i) .eqv. .true.) print *, "IEEE_FLAG error!"
       !enddo

!       Test real*4
        xr_4 = 3.0
        yr_4 = -2.0
        if (ieee_support_datatype(xr_4) .and. ieee_support_datatype(yr_4)) then
           xr_4 = ieee_copy_sign(xr_4, yr_4)
           if ( xr_4 /= -3.0 ) print *, "error 1 copy_sign failed for real*4."
        endif

        xr_4 = ieee_copy_sign(8.0, -9.0)
        if ( xr_4 /= -8.0 ) print *, "error 2 copy_sign failed for real*4."

        xr_4 = ieee_copy_sign(xr_4, 9.0)
        if ( xr_4 /= 8.0 ) print *, "error 3 copy_sign failed for real*4."

        xr_4 = ieee_copy_sign(8.0, yr_4)
        if ( xr_4 /= -8.0 ) print *, "error 4 copy_sign failed for real*4."

!       Test real*8
        xr_8 = 3.0
        yr_8 = -2.0
        if (ieee_support_datatype(xr_8) .and. ieee_support_datatype(yr_8)) then
           xr_8 = ieee_copy_sign(xr_8, yr_8)
           if ( xr_8 /= -3.0 ) print *, "error 5 copy_sign failed for real*8."
        endif

        xr_8 = ieee_copy_sign(8.0_8, -9.0_8)
        if ( xr_8 /= -8.0 ) print *, "error 6 copy_sign failed for real*8."

        xr_8 = ieee_copy_sign(xr_8, 9.0)
        if ( xr_8 /= 8.0 ) print *, "error 7 copy_sign failed for real*8."

        xr_8 = ieee_copy_sign(8.0, yr_8)
        if ( xr_8 /= -8.0 ) print *, "error 8 copy_sign failed for real*8."

!       Test different kinds
        xr_8 = -3.0
        yr_4 = 2.0
        if (ieee_support_datatype(xr_8) .and. ieee_support_datatype(yr_4)) then
           xr_8 = ieee_copy_sign(xr_8, yr_4)
           if ( xr_8 /= 3.0 ) print *, "error 9 copy_sign failed for real*8."
        endif

        xr_4 = -3.0
        yr_8 = 2.0
        if (ieee_support_datatype(xr_4) .and. ieee_support_datatype(yr_8)) then
           xr_4 = ieee_copy_sign(xr_4, yr_8)
           if ( xr_4 /= 3.0 ) print *, "error 10 copy_sign failed for real*4."
        endif

        xr_16 = -3.0
        yr_8 = 2.0
        if (ieee_support_datatype(yr_8)) then
           xr_16 = ieee_copy_sign(xr_16, yr_8)
           if ( xr_16 /= 3.0 ) print *, "error 11 copy_sign failed  for real*16."
        endif

        xr_4 = 3.0
        yr_16 = -2.0
        if (ieee_support_datatype(xr_4)) then
           xr_4 = ieee_copy_sign(xr_4, yr_16)
           if ( xr_4 /= -3.0 ) print *, "error 12 copy_sign failed for real*4."
        endif

        xr_4 = ieee_copy_sign(8.0, -9.0_8)
        if ( xr_4 /= -8.0 ) print *, "error 13 copy_sign failed for real*4."

        xr_8 = -8.0_8
        xr_8 = ieee_copy_sign(xr_8, 9.0)
        if ( xr_8 /= 8.0 ) print *, "error 14 copy_sign failed for real*8."

        xr_16 = ieee_copy_sign(8.0_16, yr_16)
        if ( xr_16 /= -8.0 ) print *, "error 15 copy_sign failed for real*16."

!       Test array of kind 4
        ysigns = -2.0
        if (ieee_support_datatype(values) .and. ieee_support_datatype(ysigns) &
     &      .and. ieee_support_datatype(xresults)) then
           xresults = ieee_copy_sign(values, ysigns)
           if (iresults(1) /= z"ffbfffff") print *, "error 16 copy_sign failed for real*4."
         if (iresults(2) /= z"ffbfffff") print *, "error 17 copy_sign failed for real*4."
         if (iresults(3) /= z"ffffffff") print *, "error 18 copy_sign failed for real*4."
         if (iresults(4) /= z"ffffffff") print *, "error 19 copy_sign failed for real*4."
         if (iresults(5) /= z"ff800000") print *, "error 20 copy_sign failed for real*4."
         if (xresults(6) /= -huge(PINF_4) ) print *, "error 21 copy_sign failed for real*4."
         if (xresults(7) /= -tiny(PINF_4) ) print *, "error 22 copy_sign failed for real*4."
         if (iresults(8) /= z"807fffff") print *, "error 23 copy_sign failed for real*4."
         if (iresults(9) /= z"80000001") print *, "error 24 copy_sign failed for real*4."
         if (iresults(10) /= z"80000000") print *, "error 25 copy_sign failed for real*4."
         if (iresults(11) /= z"80000000") print *, "error 26 copy_sign failed for real*4."
         if (iresults(12) /= z"80000001") print *, "error 27 copy_sign failed for real*4."
         if (iresults(13) /= z"807fffff") print *, "error 28 copy_sign failed for real*4."
         if (xresults(14) /= -tiny(PINF_4) ) print *, "error 29 copy_sign failed for real*4."
         if (xresults(15) /= -huge(PINF_4) ) print *, "error 30 copy_sign failed for real*4."
         if (iresults(16) /= z"ff800000") print *, "error 31 copy_sign failed for real*4."
       endif

!       Test array of kind 8
        ysigns_8 = -2.0
        if (ieee_support_datatype(values_8) .and.ieee_support_datatype(ysigns_8) &
     &      .and. ieee_support_datatype(xresults_8)) then
           xresults_8 = ieee_copy_sign(values_8, ysigns_8)
           if (iresults_8(1) /= z"fff7ffff7ff7ffff") print *, "error 32 copy_sign failed for real*8."
         if (iresults_8(2) /= z"fff7000000000000") print *, "error 33 copy_sign failed for real*8."
         if (iresults_8(3) /= z"ffffffff7ff7ffff") print *, "error 34 copy_sign failed for real*8."
         if (iresults_8(4) /= z"ffff000000000000") print *, "error 35 copy_sign failed for real*8."
         if (iresults_8(5) /= z"fff0000000000000") print *, "error 36 copy_sign failed for real*8."
         if (xresults_8(6) /= -huge(PINF_8)) print *, "error 37 copy_sign failed for real*8."
         if (xresults_8(7) /= -tiny(PINF_8)) print *, "error 38 copy_sign failed for real*8."
         if (iresults_8(8) /= z"800fffffffffffff") print *, "error 39 copy_sign failed for real*8."
         if (iresults_8(9) /= z"8000000000000001") print *, "error 40 copy_sign failed for real*8."
         if (iresults_8(10) /= z"8000000000000000") print *, "error 41 copy_sign failed for real*8."
         if (iresults_8(11) /= z"8000000000000000") print *, "error 42 copy_sign failed for real*8."
         if (iresults_8(12) /= z"8000000000000001") print *, "error 43 copy_sign failed for real*8."
         if (iresults_8(13) /= z"800fffffffffffff") print *, "error 44 copy_sign failed for real*8."
         if (xresults_8(14) /= -tiny(PINF_8) ) print *, "error 45 copy_sign failed for real*8."
         if (xresults_8(15) /= -huge(PINF_8) ) print *, "error 46 copy_sign failed for real*8."
           if (iresults_8(16) /= z"fff0000000000000") print *, "error 47 copy_sign failed for real*8."
       endif

     call ieee_get_flag(IEEE_ALL, flag_values)
       do i = 1, 5
          if (flag_values(i) .eqv. .true.) print *, "IEEE_FLAG singling in real*8 ", i, " error !"
       enddo

!       Test array of kind 16
         ysigns_16 = 2.0

     xresults_16 = ieee_copy_sign(values_16, ysigns_16)
     if (apns(1) /= z"7ff7ffff7ff7ffff") print *, "error 48 copy_sign failed for real*16."
     !if (apns(2) /= z"0000000000000000") print *, "error 49 copy_sign failed for real*16."
     if (anns(1) /= z"7ff7000000000000") print *, "error 50 copy_sign failed for real*16."
     !if (anns(2) /= z"0000000000000000") print *, "error 51 copy_sign failed for real*16."
     if (apnq(1) /= z"7fffffff7ff7ffff") print *, "error 52 copy_sign failed for real*16."
     !if (apnq(2) /= z"0000000000000000") print *, "error 53 copy_sign failed for real*16."
     if (annq(1) /= z"7fff000000000000") print *, "error 54 copy_sign failed for real*16."
     !if (annq(2) /= z"0000000000000000") print *, "error 55 copy_sign failed for real*16."

     if ( xresults_16(5) /= PINF_16 ) print *, "error 57 copy_sign failed for real*16."
       if ( xresults_16(6) /= huge(PINF_16) ) print *, "error 58 copy_sign failed for real*16."
       if ( xresults_16(7) /= tiny(PINF_16) ) print *, "error 59 copy_sign failed for real*16."
     if ( xresults_16(8) /= tiny(PINF_16) ) print *, "error 60 copy_sign failed for real*16."
       if ( xresults_16(9) /= huge(PINF_16) ) print *, "error 61 copy_sign failed for real*16."
       if ( xresults_16(10) /= z"7ff00000000000000000000000000000") print *, "error 62 copy_sign failed for real*16."

      ! Now set flags back to original.

       call ieee_get_flag(IEEE_ALL, flag_values)
       do i = 1, 5
       if ( i /= 3 ) then
              if (flag_values(i) .eqv. .true.) print *, "IEEE_FLAG singling in real*16, error ", i, " !"
        endif
       enddo

        call ieee_set_flag(ieee_all, original)

        end program

