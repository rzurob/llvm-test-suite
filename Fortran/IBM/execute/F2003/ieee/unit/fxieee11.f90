! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_REM
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
	     program fxieee11

        use ieee_arithmetic
        use constants_for_ieee

        real*4 :: xr_4, yr_4, zr_4
        real*8 :: xr_8, yr_8, zr_8
        real*16 :: xr_16, yr_16, zr_16
		integer :: n, i
		integer*8 :: n8

        real*4, dimension(4) :: xvalues = &
     &       (/6.7**23, 2.5**16, -3.6**(-3), 6.8**(20)/)
        real*4, dimension(4) :: yvalues = &
     &       (/4.22**18, -7.6**(-2), 8.1**3, 4.7**(17)/)
        real*4, dimension(4) :: results

		real*8, parameter, dimension(4) :: xvalues_8 = &
     &       (/9.8d0**115, -1.91d0**38, -2.9d0**(-3), 5.6d0**(301)/)
        real*8, parameter, dimension(4) :: yvalues_8 = &
     &       (/2.5d0**267, 0.4d0**(-9), -8.2d0**(-4), -5.61d0**(279)/)
        real*8, dimension(4) :: results_8

		real*16, parameter, dimension(4) :: xvalues_16 = &
     &       (/9.8q0**268, 1.9q0**302, -6.8q0**(-293), 3.9q0**(-289)/)
        real*16, parameter, dimension(4) :: yvalues_16 = &
     &       (/2.5q0**299, 3.4q0**(-101), 3.2q0**249, -1.3q0**(-168)/)
        real*16, dimension(4) :: results_16

        logical, dimension(5) :: flag_values

!       Test real*4
        xr_4 = 4.0
        yr_4 = 3.0
        zr_4 = ieee_rem(xr_4, yr_4)
        if (zr_4 /= 1.0 ) then
		   print *, "ieee_rem failed real*4."
		   print *, zr_4
		endif

!       Test real*8
        xr_8 = 3d0
        yr_8 = 2d0
        zr_8 = ieee_rem(xr_8, yr_8)
        if (zr_8 /= -1.0 ) then
		    print *, "ieee_rem failed real*8."
			print *, zr_8
		end if

!       test real*16
        xr_16 = 5q0
        yr_16 = 2q0
        zr_16 = ieee_rem(xr_16, yr_16)
        if (zr_16 /= 1.0 ) then
		    print *, "ieee_rem failed real*16."
			print *, zr_16
		endif

!       Test array of kind 4
        results = ieee_rem(xvalues, yvalues)
		do i = 1, 4
		   n = xvalues(i) / yvalues(i)
		   zr_4 = xvalues(i) - n *yvalues(i)
		   if (results(i) /= zr_4) then
		      print *, "ieee_rem error in kind 4 array, ", i
              print *, n, xvalues(i) / yvalues(i), xvalues(i), yvalues(i)
		      print *, results(i), zr_4
		   end if
		enddo

!       Test array of kind 8
        results_8 = ieee_rem(xvalues_8, yvalues_8)
		do i = 1, 4
		   n8 = xvalues_8(i) / yvalues_8(i)
		   zr_8 = xvalues_8(i) - n8 *yvalues_8(i)
		   if (results_8(i) /= zr_8) then
		      print *, "ieee_rem error in kind 8 array, ", i
		      print *, results_8(i), zr_8
		   end if
		enddo

!       Test array of kind 16
        results_16 = ieee_rem(xvalues_16, yvalues_16)
		do i = 1, 4
		   zr_16 = xvalues_16(i) - yvalues_16(i) * iqint(xvalues_16(i)/yvalues_16(i))
		   if (results_16(i) /= zr_16) then
		      print *, "ieee_rem error in kind 16 array, ", i
		      print *, results_16(i), zr_16
		   end if
		enddo

        end program

