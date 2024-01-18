! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh value001
! %COMPOPTS: -qfloat=nans -qfree=f90 -qstrict
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
!*  DATE                       : February 12, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_VALUE
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

	program value_val

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

!**************
!       Test real*4
!**************

        values = ieee_value(values, ctypes)

        do i = 1, 10
           if (ieee_class(values(i)) /= ctypes(i)) call zzrc(i+10)
        end do

        call ieee_get_flag(ieee_all,flag_values)

        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(100+i)
        enddo


        call ieee_set_flag(ieee_all,.false.)

!**************
!       Test real*8
!**************

        call ieee_set_flag(ieee_all,.false.)

        values_8 = ieee_value(values_8, ctypes)

        do i = 1, 10
          if (ieee_class(values_8(i)) /= ctypes(i))  call zzrc(i+20)
                           !IEEE value failed, real*8
        end do

        call ieee_get_flag(ieee_all,flag_values)

        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(200+i)
        enddo



!**************
!       Test real*16
!**************

        call ieee_set_flag(ieee_all,.false.)

         values_16 = ieee_value(values_16, ctypes)

        do i = 1, 10
            if (ieee_class(values_16(i)) /= ctypes(i))call zzrc(i+30)
                       !IEEE value failed, real*16
        end do




        end program

