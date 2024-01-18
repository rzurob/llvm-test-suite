! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_GET_ROUNDING_MODE
!*  SECONDARY FUNCTIONS TESTED : IEEE_SET_ROUNDING_MODE, IEEE_RINT
!*                               IEEE_GET_STATUS, IEEE_SET_STATUS
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
         program fxieee05

         use ieee_arithmetic
         use ieee_exceptions
         use constants_for_ieee

         real*4 :: xr(2), yr, res(2)
	     real*8 :: xr_8(2), yr_8, res_8(2)
	     real*16 :: xr_16(2), yr_16, res_16(2)
	     type(ieee_round_type) :: rtype
	     type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
	     type(ieee_round_type), parameter :: rt_20 = IEEE_TO_ZERO
	     type(ieee_round_type), parameter :: rt_up = IEEE_UP
	     type(ieee_round_type), parameter :: rt_down = IEEE_DOWN
	     type(ieee_round_type), parameter :: rt_other = IEEE_OTHER
	     type(ieee_status_type) :: status_value

	     call ieee_get_status(status_value)

!  test nereast

          call ieee_set_rounding_mode(rt_nearest)
	      call ieee_get_rounding_mode(rtype)
	      if (rtype /= rt_nearest) print *, " error in set rounding mode to nearest."
		  !  test real*4
	      yr = ieee_rint(1.1)
	      if (yr /= 1.0) print *, "ieee_rint error in rounding mode to nearest."
		  yr = ieee_rint(-23.9)
		  if (yr /= -24.0) then
	    	 print *, " ieee_rint error in rounding mode to nearest."
		     print *, yr
		  endif
		  xr(1) = 2345.89
		  xr(2) = -34367.3
		  res = ieee_rint(xr)
		  if (res(1) /= 2346.0 ) print *, " ieee_rint error in rounding mode to nearest."
		  if (res(2) /= -34367.0 ) print *, " ieee_rint error in rounding mode to nearest."

         !  test real*8
	      yr_8 = ieee_rint(1.1_8)
	      if (yr_8 /= 1.0) print *, "ieee_rint error in rounding mode to nearest."
		  yr_8 = ieee_rint(-23.9_8)
		  if (yr_8 /= -24.0) then
	    	 print *, " ieee_rint error in rounding mode to nearest."
		     print *, yr_8
		  endif
		  xr_8(1) = 2345.89_8
		  xr_8(2) = -34367.3_8
		  res_8 = ieee_rint(xr_8)
		  if (res_8(1) /= 2346.0 ) print *, " ieee_rint error in rounding mode to nearest."
		  if (res_8(2) /= -34367.0 ) print *, " ieee_rint error in rounding mode to nearest."

         !  test real*16
	      yr_16 = ieee_rint(1.1_16)
	      if (yr_16 /= 1.0) print *, "ieee_rint error in rounding mode to nearest."
		  yr_16 = ieee_rint(-23.9_16)
		  if (yr_16 /= -24.0) then
	    	 print *, " ieee_rint error in rounding mode to nearest."
		     print *, yr_16
		  endif
		  xr_16(1) = 2345.89_16
		  xr_16(2) = -34367.3_16
		  res_16 = ieee_rint(xr_16)
		  if (res_16(1) /= 2346.0 ) print *, " ieee_rint error in rounding mode to nearest."
		  if (res_16(2) /= -34367.0 ) print *, " ieee_rint error in rounding mode to nearest."

! test  to zero
            call ieee_set_rounding_mode(rt_20)
	        call ieee_get_rounding_mode(rtype)
	        if (rtype /= rt_20) print *, " error in set rounding  mode to zero."

			! test real*4
			yr = ieee_rint(1.7)
	        if (yr /= 1.0) print *, " ieee_rint error in rounding  mode to zero."
			yr = ieee_rint(-2.9)
			if (yr /= -2.0) then
			    print *, " ieee_rint error in rounding mode to zero."
				print *, yr
			 endif
          xr(1) = 2345.89
		  xr(2) = -34367.9
		  res = ieee_rint(xr)
		  if (res(1) /= 2345.0 ) print *, " ieee_rint error in rounding mode to zero."
		  if (res(2) /= -34367.0 ) print *, " ieee_rint error in rounding mode to zero."

         !  test real*8
	      yr_8 = ieee_rint(1.8_8)
	      if (yr_8 /= 1.0) print *, "ieee_rint error in rounding mode to zero."
		  yr_8 = ieee_rint(-23.9_8)
		  if (yr_8 /= -23.0) then
	    	 print *, " ieee_rint error in rounding mode to zero."
		     print *, yr_8
		  endif
		  xr_8(1) = 2345.89_8
		  xr_8(2) = -34367.63_8
		  res_8 = ieee_rint(xr_8)
		  if (res_8(1) /= 2345.0 ) print *, " ieee_rint error in rounding mode to zero."
		  if (res_8(2) /= -34367.0 ) print *, " ieee_rint error in rounding mode to zero."

         !  test real*16
	      yr_16 = ieee_rint(1.8_16)
	      if (yr_16 /= 1.0) print *, "ieee_rint error in rounding mode to zero."
		  yr_16 = ieee_rint(-23.9_16)
		  if (yr_16 /= -23.0) then
	    	 print *, " ieee_rint error in rounding mode to zero."
		     print *, yr_16
		  endif
		  xr_16(1) = 2345.89_16
		  xr_16(2) = -34367.93_16
		  res_16 = ieee_rint(xr_16)
		  if (res_16(1) /= 2345.0 ) print *, " ieee_rint error in rounding mode to zero."
		  if (res_16(2) /= -34367.0 ) print *, " ieee_rint error in rounding mode to zero."

! test +INF
            call ieee_set_rounding_mode(rt_up)
	        call ieee_get_rounding_mode(rtype)
	        if (rtype /= rt_up) print *, " error in set rounding mode to up."

           ! test real*4
			yr = ieee_rint(1.1)
	        if (yr /= 2.0) then
			   print *, "ieee_rint error in rounding mode to up."
			   print *, yr
			endif
			yr = ieee_rint(-2.6)
	        if (yr /= -2.0) then
			   print *, "ieee_rint error in rounding mode to up."
			   print *, yr
			endif
          xr(1) = 2345.12
		  xr(2) = -34367.9
		  res = ieee_rint(xr)
		  if (res(1) /= 2346.0 ) print *, " ieee_rint error in rounding mode to up."
		  if (res(2) /= -34367.0 ) print *, " ieee_rint error in rounding mode to up."

         !  test real*8
	      yr_8 = ieee_rint(1.2_8)
	      if (yr_8 /= 2.0) print *, "ieee_rint error in rounding mode to up."
		  yr_8 = ieee_rint(-23.9_8)
		  if (yr_8 /= -23.0) then
	    	 print *, " ieee_rint error in rounding mode to up."
		     print *, yr_8
		  endif
		  xr_8(1) = 2345.29_8
		  xr_8(2) = -34367.63_8
		  res_8 = ieee_rint(xr_8)
		  if (res_8(1) /= 2346.0 ) print *, " ieee_rint error in rounding mode to up."
		  if (res_8(2) /= -34367.0 ) print *, " ieee_rint error in rounding mode to up."

         !  test real*16
	      yr_16 = ieee_rint(1.2_16)
	      if (yr_16 /= 2.0) print *, "ieee_rint error in rounding mode to up."
		  yr_16 = ieee_rint(-23.9_16)
		  if (yr_16 /= -23.0) then
	    	 print *, " ieee_rint error in rounding mode to up."
		     print *, yr_16
		  endif
		  xr_16(1) = 2345.19_16
		  xr_16(2) = -34367.93_16
		  res_16 = ieee_rint(xr_16)
		  if (res_16(1) /= 2346.0 ) print *, " ieee_rint error in rounding mode to up."
		  if (res_16(2) /= -34367.0 ) print *, " ieee_rint error in rounding mode to up."

! test to -INF
	        call ieee_set_rounding_mode(rt_down)
	        call ieee_get_rounding_mode(rtype)
	        if (rtype /= rt_down) print *, " error in set rounding to down."
			! test resl*4
	        yr = ieee_rint(1.9)
	        if (yr /= 1.0) print *, " ieee_rint error in rounding to down."
			yr = ieee_rint(-2.1)
	        if (yr /= -3.0) then
			   print *, "ieee_rint error in rounding mode to down."
			   print *, yr
			endif
          xr(1) = 2345.912
		  xr(2) = -34367.29
		  res = ieee_rint(xr)
		  if (res(1) /= 2345.0 ) print *, " ieee_rint error in rounding mode to down."
		  if (res(2) /= -34368.0 ) print *, " ieee_rint error in rounding mode to down."

         !  test real*8
	      yr_8 = ieee_rint(1.82_8)
	      if (yr_8 /= 1.0) print *, "ieee_rint error in rounding mode to down."
		  yr_8 = ieee_rint(-23.19_8)
		  if (yr_8 /= -24.0) then
	    	 print *, " ieee_rint error in rounding mode to down."
		     print *, yr_8
		  endif
		  xr_8(1) = 2345.829_8
		  xr_8(2) = -34367.163_8
		  res_8 = ieee_rint(xr_8)
		  if (res_8(1) /= 2345.0 ) print *, " ieee_rint error in rounding mode to down."
		  if (res_8(2) /= -34368.0 ) print *, " ieee_rint error in rounding mode to down."

         !  test real*16
	      yr_16 = ieee_rint(1.982_16)
	      if (yr_16 /= 1.0) print *, "ieee_rint error in rounding mode to down."
		  yr_16 = ieee_rint(-23.219_16)
		  if (yr_16 /= -24.0) then
	    	 print *, " ieee_rint error in rounding mode to down."
		     print *, yr_16
		  endif
		  xr_16(1) = 2345.819_16
		  xr_16(2) = -34367.293_16
		  res_16 = ieee_rint(xr_16)
		  if (res_16(1) /= 2345.0 ) print *, " ieee_rint error in rounding mode to down."
		  if (res_16(2) /= -34368.0 ) print *, " ieee_rint error in rounding mode to down."

	     call ieee_set_status(status_value)

         end program
