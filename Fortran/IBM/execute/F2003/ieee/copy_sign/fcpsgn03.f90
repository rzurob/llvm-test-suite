! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fcpsgn03
! %COMPOPTS:   -qfree=f90  -qfloat=nofold -qstrict
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
!*                               for REAL(16).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fcpsgn03

        use ieee_arithmetic
        use constants_for_ieee

        real(4) :: xr_4, yr_4
        real(8) :: xr_8, yr_8
        real(16) :: xr_16, yr_16
        real(16), parameter, dimension(4) :: normal_val = &
     &  (/ huge(PINF_16), tiny(PINF_16), -tiny(PINF_16), -huge(PINF_16)/ )

        real(16), dimension(4) :: xnormal_res, ynormal_sgn

        logical, dimension(5) :: actual_flag_values, original(5)
        integer :: k

        equivalence (inormal_res, xnormal_res)


!...Check that all flags are false
!... if not set to .false.
        call ieee_set_flag(ieee_all,.false.)
        do k =1, 5
           if (actual_flag_values(k) .neqv. .false. ) then
              call ieee_set_flag(ieee_all(k), .false. )
           endif
        enddo

        xr_16 = 1.0_16
        yr_16 = -2.0_16

        xr_16 = ieee_copy_sign(xr_16, yr_16)
        if ( xr_16 /= -1.0_16 ) then
           error stop 1
        endif

        xr_16 = ieee_copy_sign(3.0_16, -4.0_16)
        if ( xr_16 /= -3.0_16 ) then
           error stop 2
        endif

        xr_16 = ieee_copy_sign(xr_16, 3.0_16)
        if ( xr_16 /= 3.0_16 ) then
           error stop 3
        endif

        xr_16 = ieee_copy_sign(3.0_16, yr_16)
        if ( xr_16 /= -3.0_16 ) then
           error stop 4
        endif

!...Test with arguments of different kinds
        xr_8 = -1.0_8
        yr_4 = 2.0

        xr_8 = ieee_copy_sign(xr_8, yr_4)
        if ( xr_8 /= 1.0_8 ) then
           error stop 5
        endif

        xr_4 = -1.0
        yr_8 = 2.0_8

        xr_4 = ieee_copy_sign(xr_4, yr_8)
        if ( xr_4 /= 1.0 ) then
           error stop 6
        endif

        xr_8 = 1.0_8
        yr_4 = -2.0

        xr_8 = ieee_copy_sign(xr_8, yr_4)
        if ( xr_8 /= -1.0_8 ) then
           error stop 7
        endif

        xr_4 = 1.0
        yr_8 = -2.0_8

        xr_4 = ieee_copy_sign(xr_4, yr_8)
        if ( xr_4 /= -1.0 ) then
           error stop 8
        endif


        xr_16 = -1.0_16
        yr_8 = 2.0_8

        xr_16 = ieee_copy_sign(xr_16, yr_8)
        if ( xr_16 /= 1.0_16 ) then
           error stop 9
        endif

        xr_16 = 1.0_16
        yr_8 = -2.0_8

        xr_16 = ieee_copy_sign(xr_16, yr_8)
        if ( xr_16 /= -1.0_16 ) then
           error stop 10
        endif

        xr_8 = 1.0
        yr_16 = -3.0_16

        xr_8 = ieee_copy_sign(xr_8, yr_16)
        if ( xr_8 /= -1.0 ) then
           error stop 11
        endif


        xr_8 = -1.0
        yr_16 = 3.0_16

        xr_8 = ieee_copy_sign(xr_8, yr_16)
        if ( xr_8 /= 1.0 ) then
           error stop 12
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


        ynormal_sgn = -1.0_16

        xnormal_res = ieee_copy_sign(normal_val, ynormal_sgn)
        if (xnormal_res(1) /= -huge(PINF_16) ) then
           error stop 16
        endif
        if (xnormal_res(2) /= -tiny(PINF_16) ) then
           error stop 17
        endif
        if (xnormal_res(3) /= -tiny(PINF_16) ) then
           error stop 18
        endif
        if (xnormal_res(4) /= -huge(PINF_16) ) then
           error stop 19
        endif


!...Check that no flags were turned on by IEEE_COPY_SIGN
        call ieee_get_flag(ieee_all, actual_flag_values)
        do k = 1,5
           if (actual_flag_values(k) .neqv. .false. ) then
              error stop 20
           endif
        enddo

        end




