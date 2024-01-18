! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fcpsgn04
! %COMPOPTS:  -qfree=f90 -qfloat=nofold -qstrict -qrealsize=4
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
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nofold -qstrict -qrealsize=4
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test COPY_SIGN elemental function
!*                               compiled with -qrealsize=4.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fcpsgn04

        use ieee_arithmetic
        use constants_for_ieee

        real :: xr_4, yr_4
        real(8) :: xr_8, yr_8
        real(16) :: xr_16, yr_16
        real, parameter, dimension(4) :: nan_val = &
     &  (/ PNANS_4, NNANS_4, PNANQ_4, NNANQ_4 / )

        real, parameter, dimension(2) :: inf_val = (/ PINF_4, NINF_4/ )

        real, parameter, dimension(4) :: normal_val = &
     &  (/ huge(PINF_4), tiny(PINF_4), -tiny(PINF_4), -huge(PINF_4)/ )

        real, parameter, dimension(2) :: zero_val = (/ PZERO_4, NZERO_4/)

        real, parameter, dimension(4) :: denormal_val = &
     &  (/ PHD_4, PTD_4, NTD_4, NHD_4 / )

        real, dimension(4) :: xnan_res, ynan_sgn
        real, dimension(2) :: xinf_res, yinf_sgn
        real, dimension(4) :: xnormal_res, ynormal_sgn
        real, dimension(2) :: xzero_res, yzero_sgn
        real, dimension(4) :: xdenorm_res, ydenorm_sgn

        integer*4, dimension(4) :: inan_res
        integer*4, dimension(2) :: iinf_res
        integer*4, dimension(4) :: inormal_res
        integer*4, dimension(2) :: izero_res
        integer*4, dimension(4) :: idenorm_res


        logical, dimension(5) :: actual_flag_values, original(5)
        integer :: k, caseid

        equivalence (inan_res, xnan_res)
        equivalence (iinf_res, xinf_res)
        equivalence (inormal_res, xnormal_res)
        equivalence (izero_res, xzero_res)
        equivalence (idenorm_res, xdenorm_res)

        caseid = 1

!...Check that all flags are false
!... if not set to .false.
        call ieee_set_flag(ieee_all,.false.)
        do k =1, 5
           if (actual_flag_values(k) .neqv. .false. ) then
              call ieee_set_flag(ieee_all(k), .false. )
           endif
        enddo

        xr_4 = 1.0
        yr_4 = -2.0
        if (ieee_support_datatype(xr_4) .and. ieee_support_datatype(yr_4)) then
           xr_4 = ieee_copy_sign(xr_4, yr_4)
           if ( xr_4 /= -1.0 ) then
              error stop 1
           endif
        endif
        xr_4 = ieee_copy_sign(3.0, -4.0)
        if ( xr_4 /= -3.0 ) then
           error stop 2
        endif

        xr_4 = ieee_copy_sign(xr_4, 3.0)
        if ( xr_4 /= 3.0 ) then
           error stop 3
        endif

        xr_4 = ieee_copy_sign(3.0, yr_4)
        if ( xr_4 /= -3.0 ) then
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

        xr_4 = 1.0
        yr_16 = -3.0_16
        if (ieee_support_datatype(xr_4) .and. ieee_support_datatype(yr_16)) then
           xr_4 = ieee_copy_sign(xr_4, yr_16)
           if ( xr_4 /= -1.0 ) then
              error stop 11
           endif
        endif

        xr_4 = -1.0
        yr_16 = 3.0_16
        if (ieee_support_datatype(xr_4) .and. ieee_support_datatype(yr_16)) then
           xr_4 = ieee_copy_sign(xr_4, yr_16)
           if ( xr_4 /= 1.0 ) then
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
        ynan_sgn = -1.0
      if (ieee_support_datatype(nan_val) .and. ieee_support_datatype(ynan_sgn) &
     &      .and. ieee_support_datatype(xnan_res)) then
           xnan_res = ieee_copy_sign(nan_val, ynan_sgn)

           if (inan_res(1) /= z"ffbfffff") then
              error stop 16
           endif
           if (inan_res(2) /= z"ffbfffff") then
              error stop 17
           endif
           if (inan_res(3) /= z"ffffffff") then
              error stop 18
           endif
           if (inan_res(4) /= z"ffffffff") then
              error stop 19
           endif
        endif

        yinf_sgn = -1.1
      if (ieee_support_datatype(inf_val) .and. ieee_support_datatype(yinf_sgn) &
     &      .and. ieee_support_datatype(xinf_res)) then
           xinf_res = ieee_copy_sign(inf_val, yinf_sgn)
           if (iinf_res(1) /= z"ff800000") then
              error stop 20
           endif
           if (iinf_res(2) /= z"ff800000") then
              error stop 21
           endif
      endif

        ynormal_sgn = -1.0
       if (ieee_support_datatype(normal_val)  &
     &   .and. ieee_support_datatype(xnormal_res)) then
           xnormal_res = ieee_copy_sign(normal_val, ynormal_sgn)
           if (xnormal_res(1) /= -huge(PINF_4) ) then
              error stop 22
           endif
           if (xnormal_res(2) /= -tiny(PINF_4) ) then
              error stop 23
           endif
           if (xnormal_res(3) /= -tiny(PINF_4) ) then
              error stop 24
           endif
           if (xnormal_res(4) /= -huge(PINF_4) ) then
              error stop 25
           endif
       endif

       yzero_sgn = -1.1
        if (ieee_support_datatype(zero_val) .and.  &
     &      ieee_support_datatype(xzero_res)) then
           xzero_res = ieee_copy_sign(zero_val, yzero_sgn)
           if (izero_res(1) /= z"80000000") then
              error stop 26
           endif
           if (izero_res(2) /= z"80000000") then
              error stop 27
           endif
        endif

        ydenorm_sgn = -1.0
        if (ieee_support_datatype(denormal_val) .and.  &
     &      ieee_support_datatype(xdenorm_res)) then
           xdenorm_res = ieee_copy_sign(denormal_val, ydenorm_sgn)

           if (xdenorm_res(1) /= z"807fffff") then
              error stop 28
           endif
           if (idenorm_res(2) /= z"80000001") then
              error stop 29
           endif
           if (idenorm_res(3) /= z"80000001") then
              error stop 30
           endif
           if (idenorm_res(4) /= z"807fffff") then
              error stop 31
           endif
        endif

!...Test with arrays and positive second argument
        ynan_sgn = 1.0
      if (ieee_support_datatype(nan_val) .and. ieee_support_datatype(ynan_sgn) &
     &      .and. ieee_support_datatype(xnan_res)) then
           xnan_res = ieee_copy_sign(nan_val, ynan_sgn)
           if (inan_res(1) /= z"7fbfffff") then
              error stop 32
           endif
           if (inan_res(2) /= z"7fbfffff") then
              error stop 33
           endif
           if (inan_res(3) /= z"7fffffff") then
              error stop 34
           endif
           if (inan_res(4) /= z"7fffffff") then
              error stop 35
           endif
        endif

        yinf_sgn = 1.1
      if (ieee_support_datatype(inf_val) .and. ieee_support_datatype(yinf_sgn) &
     &      .and. ieee_support_datatype(xinf_res)) then
           xinf_res = ieee_copy_sign(inf_val, yinf_sgn)
           if (iinf_res(1) /= z"7f800000") then
              error stop 36
           endif
           if (iinf_res(2) /= z"7f800000") then
              error stop 37
           endif
      endif

        ynormal_sgn = 1.0
       if (ieee_support_datatype(normal_val)  &
     &   .and. ieee_support_datatype(xnormal_res)) then
           xnormal_res = ieee_copy_sign(normal_val, ynormal_sgn)
           if (xnormal_res(1) /= huge(PINF_4) ) then
              error stop 38
           endif
           if (xnormal_res(2) /= tiny(PINF_4) ) then
              error stop 39
           endif
           if (xnormal_res(3) /= tiny(PINF_4) ) then
              error stop 40
           endif
           if (xnormal_res(4) /= huge(PINF_4) ) then
              error stop 41
           endif
       endif

       yzero_sgn = 1.1
        if (ieee_support_datatype(zero_val) .and.  &
     &       ieee_support_datatype(xzero_res)) then
           xzero_res = ieee_copy_sign(zero_val, yzero_sgn)
           if (izero_res(1) /= z"00000000") then
              error stop 42
           endif
           if (izero_res(2) /= z"00000000") then
              error stop 43
           endif
        endif

        ydenorm_sgn = 1.0
        if (ieee_support_datatype(denormal_val) .and.  &
     &      ieee_support_datatype(xdenorm_res)) then
           xdenorm_res = ieee_copy_sign(denormal_val, ydenorm_sgn)

           if (idenorm_res(1) /= z"007fffff") then
              error stop 44
           endif
           if (idenorm_res(2) /= z"00000001") then
              error stop 45
           endif
           if (idenorm_res(3) /= z"00000001") then
              error stop 46
           endif
           if (idenorm_res(4) /= z"007fffff") then
              error stop 47
           endif
        endif

!...Check that no flags were turned on by IEEE_COPY_SIGN
        call ieee_get_flag(ieee_all, actual_flag_values)
        do k = 1,5
           if (actual_flag_values(k) .neqv. .false. ) then
              error stop 48
           endif
        enddo

        end




