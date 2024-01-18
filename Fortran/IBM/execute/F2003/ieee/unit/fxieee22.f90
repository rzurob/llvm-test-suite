! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee22 
! %COMPOPTS: -qfloat=nans -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Marcus Yu
!*  DATE                       : February 12, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_VALUE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee22

        use ieee_arithmetic
        use constants_for_ieee

        real*4, dimension(10) :: values
        real*8, dimension(10) :: values_8
		real*16, dimension(10) :: values_16

        logical, dimension(5) :: flag_values
        integer :: i

        type(ieee_class_type), parameter :: ctypes(10) = &
     &  (/                      &
     &  ieee_signaling_nan,     &
     &  ieee_quiet_nan,         &
     &  ieee_negative_inf,      &
     &  ieee_positive_inf,      &
     &  ieee_negative_normal,   &
     &  ieee_positive_normal,   &
     &  ieee_negative_denormal, &
     &  ieee_positive_denormal, &
     &  ieee_negative_zero,     &
     &  ieee_positive_zero      &
     &   /)

!       ieee_value should not set any flags.  Clear all flags and
!       check at the end that all flags are clear.
        call ieee_set_flag(ieee_all,.false.)

!       Test real*4
        values = ieee_value(values, ctypes)
!        do i = 1, 10
 !          if (ieee_class(values(i)) /= ctypes(i)) then
 !             print *, "IEEE value failed, real*4, at ", i, "th number."
 !          endif
 !       end do

  !      values(1) = ieee_value(values(1), ieee_quiet_nan)
  !      if (ieee_class(values(1)) /= ieee_quiet_nan) then
  !         print *, "IEEE value failed, real*4, nanq"
  !      endif
     
	!    values(1) = ieee_value(values(1), ieee_positive_zero)
    !    if (ieee_class(values(1)) /= ieee_positive_zero) then
    !       print *, "IEEE value failed, real*4, p_zero"
    !    endif

!       Test real*8
    !    values_8 = ieee_value(values_8, ctypes)
    !    do i = 1, 10
   !        if (ieee_class(values_8(i)) /= ctypes(i)) then
   !           print *, "IEEE value failed, real*8, at ", i, "th number."
   !        endif
   !     end do

  !      values_8(1) = ieee_value(values_8(1), ieee_positive_inf)
  !      if (ieee_class(values_8(1)) /= ieee_positive_inf) then
  !        print *, "IEEE value failed, real*8, inf"
  !      endif

!       Test real*16
!         values_16 = ieee_value(values_16, ctypes)
 !       do i = 1, 10
 !           if (ieee_class(values_16(i)) /= ctypes(i)) then
!		       print *, "IEEE value failed, real*16, at ", i
!          endif
 !       end do

 !       values_16(1) = ieee_value(values_16(1), ieee_positive_inf)
 !       if (ieee_class(values_16(1)) /= ieee_positive_inf) then
  !         print *, "IEEE value failed, real*16, at inf."
  !      endif

! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1, 5
           if (flag_values(i) .neqv. .false.) then
               print *, "ieee_class failed: An exception flag (",i,") was set."
           endif
        enddo

        end program

