! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fcpsgn02
! %COMPOPTS:   -qfree=f90 -qfloat=nofold -qstrict
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
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_COPY_SIGN
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nofold -qstrict
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test COPY_SIGN elemental function
!*                               for REAL(8).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fcpsgn02

        use ieee_arithmetic
        use constants_for_ieee

        real(4) :: xr_4, yr_4
        real(8) :: xr_8, yr_8
        real(16) :: xr_16, yr_16

        real(8), parameter, dimension(2) :: inf_val = (/ PINF_8, NINF_8/ )

        real(8), parameter, dimension(4) :: normal_val = &
     &  (/ huge(PINF_8), tiny(PINF_8), -tiny(PINF_8), -huge(PINF_8)/ )

        real(8), parameter, dimension(2) :: zero_val = (/ PZERO_8, NZERO_8/)

        real(8), parameter, dimension(4) :: denormal_val = &
     &  (/ PHD_8, PTD_8, NTD_8, NHD_8 / )

        real(8), dimension(2) :: xinf_res, yinf_sgn
        real(8), dimension(4) :: xnormal_res, ynormal_sgn
        real(8), dimension(2) :: xzero_res, yzero_sgn
        real(8), dimension(4) :: xdenorm_res, ydenorm_sgn

        integer*8, dimension(2) :: iinf_res
        integer*8, dimension(4) :: inormal_res
        integer*8, dimension(2) :: izero_res
        integer*8, dimension(4) :: idenorm_res


        logical, dimension(5) :: actual_flag_values
        integer :: k

        equivalence (iinf_res, xinf_res)
        equivalence (inormal_res, xnormal_res)
        equivalence (izero_res, xzero_res)
        equivalence (idenorm_res, xdenorm_res)


!...Check that all flags are false
!... if not set to .false.
        call ieee_set_flag(ieee_all,.false.)
        do k =1, 5
           if (actual_flag_values(k) .neqv. .false. ) then
              call ieee_set_flag(ieee_all(k), .false. )
           endif
        enddo

        xr_8 = 1.0_8
        yr_8 = -2.0_8
        if (ieee_support_datatype(xr_8) .and. ieee_support_datatype(yr_8)) then
           xr_8 = ieee_copy_sign(xr_8, yr_8)
           if ( xr_8 /= -1.0_8 ) then
              error stop 1
           endif
        endif
        xr_8 = ieee_copy_sign(3.0_8, -4.0_8)
        if ( xr_8 /= -3.0_8 ) then
           error stop 2
        endif

        xr_8 = ieee_copy_sign(xr_8, 3.0_8)
        if ( xr_8 /= 3.0_8 ) then
           error stop 3
        endif

        xr_8 = ieee_copy_sign(3.0_8, yr_8)
        if ( xr_8 /= -3.0_8 ) then
           error stop 4
        endif

!...Test with arguments of different kinds
        xr_8 = -1.0_8
        yr_4 = 2.0
        if (ieee_support_datatype(xr_8) .and. ieee_support_datatype(yr_4)) then
           xr_8 = ieee_copy_sign(xr_8, yr_4)
           if ( xr_8 /= 1.0_8 ) then
              error stop 5
           endif
        endif

        xr_4 = -1.0
        yr_8 = 2.0_8
        if (ieee_support_datatype(xr_4) .and. ieee_support_datatype(yr_8)) then
           xr_4 = ieee_copy_sign(xr_4, yr_8)
           if ( xr_4 /= 1.0 ) then
              error stop 6
           endif
        endif

        xr_8 = 1.0_8
        yr_4 = -2.0
        if (ieee_support_datatype(xr_8) .and. ieee_support_datatype(yr_4)) then
           xr_8 = ieee_copy_sign(xr_8, yr_4)
           if ( xr_8 /= -1.0_8 ) then
              error stop 7
           endif
        endif

        xr_4 = 1.0
        yr_8 = -2.0_8
        if (ieee_support_datatype(xr_4) .and. ieee_support_datatype(yr_8)) then
           xr_4 = ieee_copy_sign(xr_4, yr_8)
           if ( xr_4 /= -1.0 ) then
              error stop 8
           endif
        endif

        xr_16 = -1.0_16
        yr_8 = 2.0_8
        if (ieee_support_datatype(xr_16) .and. ieee_support_datatype(yr_8)) then
           xr_16 = ieee_copy_sign(xr_16, yr_8)
           if ( xr_16 /= 1.0_16 ) then
              error stop 9
           endif
        endif

        xr_16 = 1.0_16
        yr_8 = -2.0_8
        if (ieee_support_datatype(xr_16) .and. ieee_support_datatype(yr_8)) then
           xr_16 = ieee_copy_sign(xr_16, yr_8)
           if ( xr_16 /= -1.0_16 ) then
              error stop 10
           endif
        endif

        xr_8 = 1.0
        yr_16 = -3.0_16
        if (ieee_support_datatype(xr_8) .and. ieee_support_datatype(yr_16)) then
           xr_8 = ieee_copy_sign(xr_8, yr_16)
           if ( xr_8 /= -1.0 ) then
              error stop 11
           endif
        endif

        xr_8 = -1.0
        yr_16 = 3.0_16
        if (ieee_support_datatype(xr_8) .and. ieee_support_datatype(yr_16)) then
           xr_8 = ieee_copy_sign(xr_8, yr_16)
           if ( xr_8 /= 1.0 ) then
              error stop 12
           endif
        endif

        xr_8 = ieee_copy_sign(1.0_8, -2.0)
        if ( xr_8 /= -1.0_8 ) then
           error stop 13
        endif

        xr_8 = -2.0_8
        xr_8 = ieee_copy_sign(xr_8, 7.0)
        if ( xr_8 /= 2.0_8 ) then
           error stop 14
        endif

        xr_16 = -2.0_16
        xr_16 = ieee_copy_sign(xr_16, 7.0)
        if ( xr_16 /= 2.0_16 ) then
           error stop 15
        endif

!...Test with arrays and negative second argument

        yinf_sgn = -1.1_8
      if (ieee_support_datatype(inf_val) .and. ieee_support_datatype(yinf_sgn) &
     &      .and. ieee_support_datatype(xinf_res)) then
           xinf_res = ieee_copy_sign(inf_val, yinf_sgn)
           if (iinf_res(1) /= z"fff0000000000000") then
              error stop 16
           endif
           if (iinf_res(2) /= z"fff0000000000000") then
              error stop 17
           endif
      endif

        ynormal_sgn = -1.0_8
       if (ieee_support_datatype(normal_val)  &
     &   .and. ieee_support_datatype(xnormal_res)) then
           xnormal_res = ieee_copy_sign(normal_val, ynormal_sgn)
           if (xnormal_res(1) /= -huge(PINF_8) ) then
              error stop 18
           endif
           if (xnormal_res(2) /= -tiny(PINF_8) ) then
              error stop 19
           endif
           if (xnormal_res(3) /= -tiny(PINF_8) ) then
              error stop 20
           endif
           if (xnormal_res(4) /= -huge(PINF_8) ) then
              error stop 21
           endif
       endif

       yzero_sgn = -1.1_8
        if (ieee_support_datatype(zero_val) .and.  &
     &      ieee_support_datatype(xzero_res)) then
           xzero_res = ieee_copy_sign(zero_val, yzero_sgn)
           if (izero_res(1) /= z"8000000000000000") then
              error stop 22
           endif
           if (izero_res(2) /= z"8000000000000000") then
              error stop 23
           endif
        endif

        ydenorm_sgn = -1.0_8
        if (ieee_support_datatype(denormal_val) .and.  &
     &      ieee_support_datatype(xdenorm_res)) then
           xdenorm_res = ieee_copy_sign(denormal_val, ydenorm_sgn)
           if (idenorm_res(1) /= z"800fffffffffffff") then
              error stop 24
           endif
           if (idenorm_res(2) /= z"8000000000000001") then
              error stop 25
           endif
           if (idenorm_res(3) /= z"8000000000000001") then
              error stop 26
           endif
           if (idenorm_res(4) /= z"800fffffffffffff") then
              error stop 27
           endif
        endif

!...Check that no flags were turned on by IEEE_COPY_SIGN
        call ieee_get_flag(ieee_all, actual_flag_values)
        do k = 1,5
           if (actual_flag_values(k) .neqv. .false. ) then
              error stop 28
           endif
        enddo

        end




