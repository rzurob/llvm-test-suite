! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 5, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_GET_FLAG
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
	     program fxieee03

         use ieee_exceptions
         use constants_for_ieee

         real*4 :: xr, yr, zr, tmp
		 real*8 :: xr_8, yr_8, zr_8, r8, tmp_8
		 real*16 :: xr_16, yr_16, zr_16, r16, tmp_16
         logical :: f_value
         logical, dimension(5) :: flag_values
		 integer :: i

         common xr, yr, zr, tmp
         common xr_8, yr_8, zr_8, r8, tmp_8
         common xr_16, yr_16, zr_16, r16, tmp_16

	     type(ieee_status_type) :: status_value

	     call ieee_get_status(status_value)
	     flag_values = .false.
		 call ieee_set_halting_mode(ieee_all, flag_values)
		 ! flag_values = .true.

!  test real*4
         ! print *, "test real*4"
         xr = 3.0
	     yr = 0.0
	     zr = xr / yr
		 f_value = .false.
	     call ieee_get_flag(IEEE_DIVIDE_BY_ZERO, f_value)
		 tmp = zr
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_DIVIDE_BY_ZERO!"
		 call ieee_set_flag(IEEE_DIVIDE_BY_ZERO, .false.)

	     xr = 0.0
	     zr = xr / yr
	     call ieee_get_flag(IEEE_INVALID, f_value)
		 tmp = zr
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_INVALID!"
		 call ieee_set_flag(IEEE_INVALID, .false.)

	     zr = huge(1.0)
		 xr = huge(1.0)
		 zr = zr * xr
		 tmp = zr
	     call ieee_get_flag(IEEE_OVERFLOW, f_value)
		 if (f_value .eqv. .false.) print *, "flag error in IEEE_OVERFLOW!"
		 call ieee_set_flag(IEEE_OVERFLOW, .false.)

		 xr = tiny(1.0)
		 zr = xr - xr*0.2
	     call ieee_get_flag(IEEE_UNDERFLOW, f_value)
		 tmp = zr
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_UNDERFLOW!"
         call ieee_set_flag(IEEE_UNDERFLOW, .false.)

         r8 = xr
	     call ieee_get_flag(IEEE_INEXACT, f_value)
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_INEXACT!"
	     call ieee_set_flag(IEEE_INEXACT, .false.)

!  test real*8
         ! print *, "test real*8"
         xr_8 = 3.0_8
	     yr_8 = 0.0_8
	     zr_8 = xr_8 / yr_8
		 tmp_8 = zr_8
	     call ieee_get_flag(IEEE_DIVIDE_BY_ZERO, f_value)
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_DIVIDE_BY_ZERO real*8!"
		 call ieee_set_flag(IEEE_DIVIDE_BY_ZERO, .false.)

	     xr_8 = 0.0_8
	     zr_8 = xr_8 / yr_8
		 tmp_8 = zr_8
	     call ieee_get_flag(IEEE_INVALID, f_value)
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_INVALID real*8!"
		 call ieee_set_flag(IEEE_INVALID, .false.)

	     xr_8 = huge(1.0_8)
		 zr_8 =  2.0*xr_8
		 tmp_8 = zr_8
		 call ieee_get_flag(IEEE_OVERFLOW, f_value)
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_OVERFLOW real*8!"
		 call ieee_set_flag(IEEE_OVERFLOW, .false.)

		 xr_8 = tiny(1.0_8)
	     zr_8 = xr_8*xr_8
		 tmp_8 = zr_8
	     call ieee_get_flag(IEEE_UNDERFLOW, f_value)
		 if (f_value .eqv. .false.) print *, "flag error in IEEE_UNDERFLOW real*8!"
         call ieee_set_flag(IEEE_UNDERFLOW, .false.)

         r16 = xr_8
	     call ieee_get_flag(IEEE_INEXACT, f_value)
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_INEXACT real*8!"
         call ieee_set_flag(IEEE_INEXACT, .false.)

!  test real*16
         ! print *, "test real*16"
         xr_16 = 3.0_16
	     yr_16 = 0.0_16
	     zr_16 = xr_16 / yr_16
		 call ieee_get_flag(IEEE_DIVIDE_BY_ZERO, f_value)
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_DIVIDE_BY_ZERO!"

	     xr_16 = 0.0_16
	     zr_16 = xr_16 / yr_16
	     call ieee_get_flag(IEEE_INVALID, f_value)
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_INVALID!"

		 xr_16 = huge(PINF_16)
	     zr_16 = 2.0*xr_16
	     call ieee_get_flag(IEEE_OVERFLOW, f_value)
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_OVERFLOW!"

	     xr_16 = tiny(PINF_16)
	     zr_16 = xr_16*xr_16
	     call ieee_get_flag(IEEE_UNDERFLOW, f_value)
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_UNDERFLOW!"

         r16 = xr_4
	     call ieee_get_flag(IEEE_INEXACT, f_value)
	     if (f_value .eqv. .false.) print *, "flag error in IEEE_INEXACT!"

         call ieee_get_flag(IEEE_ALL, flag_values)
		 do i = 1, 5
	       if (flag_values(i) .eqv. .false.) print *, "Error, IEEE_EXCEPTION ", i, " was changed!"
	     enddo

! Now set the flags back.

        call ieee_set_status(status_value)

        end program

