! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 11, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SUPPORT dtyp, denormal, divide
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
	     program fxieee15

        use ieee_arithmetic
		use ieee_exceptions
        use constants_for_ieee

        logical(4) :: l_flag, original(5), f_values(5)

        integer :: i

        real*4, parameter, dimension(2) :: values = &
     &       (/             &
     &       huge(1.0),     & ! Positive Normal
     &       tiny(1.0)      & ! Positive Normal
     &       /)

    	  real*8, parameter, dimension(2) :: values_8 = &
     &       (/             &
     &       huge(1.0_8),  & ! Positive Normal
     &       tiny(1.0_8)   & ! Positive Normal
     &       /)

        real*16, parameter, dimension(2) :: values_16 = &
     &       (/              &
     &       huge(1.0_16),  & ! Positive Normal
     &       tiny(1.0_16)   & ! Positive Normal
     &       /)


        ! get original flags
        call ieee_get_flag(ieee_all, original)
	!	call ieee_set_flag(ieee_all, .false.)

! test ieee_support_datatype
        if (ieee_support_datatype()) print *, "ieee datatype error."

        do i = 1, 2
		     if (ieee_support_datatype(values(i)) .neqv. .true.) then
		        print *, "ieee datatype error in real*4."
		     endif
        enddo

        do i = 1, 2
		      if (ieee_support_datatype(values_8(i)) .neqv. .true.) then
		         print *, "ieee datatype error in real*8."
		      endif
        enddo

        do i = 1, 2
           if (ieee_support_datatype(values_16(i))) then
              print *, "ieee datatype error in real*16."
           endif
        enddo

		if (ieee_support_datatype(values) .neqv. .true.) then
		    print *, "ieee datatype error in real*4."
		endif

		if (ieee_support_datatype(values_8) .neqv. .true.) then
           print *, "ieee datatype error in real*8."
		endif

        if (ieee_support_datatype(values_16)) then
		   print *, "ieee datatype error in real*16."
        endif

! test ieee_support_denormal
        if (ieee_support_denormal()) print *, "denormal error."

        do i = 1, 2
		  l_flag = ieee_support_denormal(values(i))
		   if (l_flag .eqv. .false.) then
		      print *, "denormal error in real*4."
		   end if
        enddo

        do i = 1, 2
		   l_flag = ieee_support_denormal(values_8(i))
		   if (l_flag .eqv. .false.) then
			  print *, "denormal error in real*8."
		   end if
        enddo

        do i = 1, 2
		  l_flag = ieee_support_denormal(values_16(i))
		   if (l_flag .eqv. .true.) then
		      print *, "denormal error in real*16."
		   end if
        enddo

        if (ieee_support_denormal(values) .eqv. .false.) then
		   print *, "denormal error in real*4."
        endif

        if (ieee_support_denormal(values_8) .eqv. .false.) then
		   print *, "denormal error in real*8."
        endif

        if (ieee_support_denormal(values_16) .eqv. .true.) then
		   print *, "denormal error in real*16."
        endif

! test support_divide
        if (ieee_support_divide()) print *, "divide error."

        do i = 1, 2
		  	  if (ieee_support_divide(values(i)) .neqv. .true.) then
		  	     print *, "divide error in real*4."
           end if
        enddo

        do i = 1, 2
		  	  if (ieee_support_divide(values_8(i)) .neqv. .true.) then
		  	     print *, "divide error in real*8."
           end if
        enddo

        do i = 1, 2
		   if (ieee_support_divide(values_16(i)) .eqv. .true.) then
		  	   print *, "divide error in real*16."
           end if
        enddo

        if (ieee_support_divide(values) .neqv. .true.) then
           print *, "divide error in real*4."
        end if

		if (ieee_support_divide(values_8) .neqv. .true.) then
		   print *, "divide error in real*8."
        end if

        if (ieee_support_divide(values_16) .eqv. .true.) then
		   print *, "divide error in real*16."
        end if

		call ieee_get_flag(ieee_all, f_values)
		do i = 1, 5
		   if (f_values(i) .eqv. .true.) print *, "error, exception ", i, " was set"
		end do
        ! se flags back to original
        call ieee_set_flag(ieee_all, original)

        end
