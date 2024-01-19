! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SCALB
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
	     program fxieee12

        use ieee_arithmetic

        real*4 :: xr_4, yr_4, xra4(2), yra4(2)
        integer :: zi, za(2), i
		real*8 :: xr_8, yr_8, xra8(2), yra8(2)
        real*16 :: xr_16, yr_16, xra16(2), yra16(2)

        logical :: flag_value
        type(ieee_status_type) :: status_value

	    call ieee_get_status(status_value)

!       Test real*4
        xr_4 = 4.0
        zi = 2
        yr_4 = ieee_scalb(xr_4, zi)
		xr_4 = 2**zi*xr_4
        if (yr_4 /= xr_4 ) print *, "ieee_scalb failed for real*4."

		do i = 1, 15
		   yr_4 = ieee_scalb(real(2**i), i)
		   xr_4 = 2**i*2**i
		   if ( yr_4 /= xr_4 ) then
		       print *, yr_4, xr_4
		   endif
		enddo

		xra4 = (/16, 18/)
		za = (/8, 19/)
		yra4 = ieee_scalb(xra4, za)
		do i = 1, 2
		   xra4(i) = 2**za(i)*xra4(i)
		   if ( yra4(i) /= xra4(i) ) print *,"ieee_scalb failed for real*4 in array ", i
        enddo

!       Test real*8
        xr_8 = 3.0_8
        zi = 2
        yr_8 = ieee_scalb(xr_8, zi)
		xr_8 = 2**zi*xr_8
        if (yr_8 /= xr_8 ) print *, "ieee_scalb failed for real*8."

		do i = 1, 31
		   yr_8 = 2_8**i
		   yr_8 = ieee_scalb(yr_8, i)
		   xr_8 = 2_8**i*2_8**i
		   if ( yr_8 /= xr_8 ) then
		       print *, i, yr_8, xr_8
		   endif
		enddo

		xra8 = (/16_8, 18_8/)
		za = (/12, 29/)
		yra8 = ieee_scalb(xra8, za)
		do i = 1, 2
		   xra8(i) = 2**za(i)*xra8(i)
		   if ( yra8(i) /= xra8(i) ) print *,"ieee_scalb failed for real*8 in array ", i
        enddo

!    test real*16
        xr_16 = 5.0_16
        zi = 5
        yr_16 = ieee_scalb(xr_16, zi)
		xr_16 = 2**zi*xr_16
        if (yr_16 /= xr_16 ) print *, "ieee_scalb failed for real*16."

		do i = 1, 62
		   yr_16 = 2.0_16**i
		   yr_16 = ieee_scalb(yr_16, i)
		   xr_16 = 2.0_16**i*2.0_16**i
		   if ( yr_16 /= xr_16 ) then
		       print *, i, yr_16, xr_16
		   endif
		enddo

        xra16 = (/126.0_16, 168.0_16/)
		za = (/9, 18/)
		yra16 = ieee_scalb(xra16, za)
		do i = 1, 2
		   xra16(i) = (2**za(i))*xra16(i)
		   if ( yra16(i) /= xra16(i) ) print *,"ieee_scalb failed for real*16 in array ", i
        enddo

        xr_4 = z"7f800000"
        zi = 56
        yr_4 = ieee_scalb(xr_4, zi)
	    if (yr_4 /= z"7f800000")  print *, "ieee_scalb failed at PINF."
        call ieee_get_flag(IEEE_OVERFLOW, flag_value)
	    if (flag_value .eqv. .true.) print *, "IEEE_OVERFLOW signaling for PINF!"

        zi = 2147483647
		xr_4 = 16
		yr_4 = ieee_scalb(xr_4, zi)
		if (yr_4 /= z"7f800000")  print *, "ieee_scalb failed for big +i."
		call ieee_get_flag(IEEE_OVERFLOW, flag_value)
	    if (flag_value .eqv. .false.) print *, "IEEE_OVERFLOW not signaling for big +i!"

        zi = -9
		xr_4 = z"ff800000"
		yr_4 = ieee_scalb(xr_4, zi)
		if (yr_4 /= z"ff800000")  then
		   print *, "ieee_scalb failed for big -i."
		   print '(z8.8)',yr_4 , zi , xr_4
		endif
		call ieee_get_flag(IEEE_OVERFLOW, flag_value)
	    if (flag_value .eqv. .false.) print *, "IEEE_OVERFLOW not signaling for big -i!"

        xr_4 = tiny(1.0)
		! print '(z8.8)', xr_4
        zi  = -24
        yr_4 = ieee_scalb(xr_4, zi)
		! print '(z8.8)', yr_4
		xr_4 = 2**zi*xr_4
		if (yr_4 /= xr_4) then
 		   print '(z8.8)', yr_4, xr_4
		endif
        call ieee_get_flag(IEEE_UNDERFLOW, flag_value)
        if (flag_value .eqv. .false.) print *, "IEEE_UNDERFLOW not signaling!"

        call ieee_set_status(status_value)

        end program

