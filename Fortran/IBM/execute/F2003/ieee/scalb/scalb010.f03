! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SCALB with arrays.
!*                               Only real*8 will be promoted.
!*
!*  SECONDARY FUNCTIONS TESTED : -qautodbl=dblpad8
!*
!*  REQUIRED COMPILER OPTIONS  : -qautodbl=dblpad8
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : IEEE_SCALB(x,i) = 2**i*x
!*  where the function arguments are real arrays.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee12

        use ieee_arithmetic

        real*4 :: xar4(2), result4(2)
        integer :: iar(2), i, Z, P, R
	real*8 ::  xar8(2), result8(2)
        real*16 :: xar16(2), result16(2)
        integer :: iresult16(2), ixar16(2), iresult8(2), ixar8(2)

        equivalence(iresult8(2), ixar8(2))
        equivalence(iresult16(2), ixar16(2))
        logical :: flag_values(5)
        type(ieee_status_type) :: status_value

	call ieee_set_flag(ieee_all,.false.)

!       Test real*4

	xar4 = (/16, 18/)
        P = precision(xar4(1))
        R = range(xar4(1))
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 4) error stop 201

        P = precision(xar4(2))
        R = range(xar4(2))
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 4) error stop 202

	iar = (/8, 19/)
	result4 = ieee_scalb(xar4, iar)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 101
        enddo

	do i = 1, 2
	   xar4(i) = 2.0_4**iar(i)*xar4(i)
	   if ( result4(i) /= xar4(i) ) error stop 4
        enddo

!       Test real*8

        call ieee_set_flag(ieee_all,.false.)

	xar8 = (/16_8, 18_8/)
        P = precision(xar8(1))
        R = range(xar8(1))
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 16) error stop 203

        P = precision(xar8(2))
        R = range(xar8(2))
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 16) error stop 204


	iar = (/12, 29/)
	result8 = ieee_scalb(xar8, iar)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 201
        enddo

	do i = 1, 2
	   xar8(i) = 2.0_8**iar(i)*xar8(i)
	   if ( iresult8(i) /= ixar8(i) ) error stop 8
        enddo

!    test real*16

         call ieee_set_flag(ieee_all,.false.)

        xar16 = (/126.0_16, 168.0_16/)
        P = precision(xar16(1))
        R = range(xar16(1))
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 16) error stop 205

        P = precision(xar16(2))
        R = range(xar16(2))
        Z = ieee_selected_real_kind(P,R)
        if (Z /= 16) error stop 206

	iar = (/9, 8/)
	result16 = ieee_scalb(xar16, iar)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 301
        enddo


	 do i = 1, 2
	   xar16(i) = (2.0_16**iar(i))*xar16(i)
	   if ( iresult16(i) /= ixar16(i) ) error stop 16
        enddo

        end program
