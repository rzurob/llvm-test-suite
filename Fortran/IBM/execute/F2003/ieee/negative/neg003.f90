! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh neg003 
! %COMPOPTS: -qfree=f90 -qxlf90=signedzero
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.mod
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Alexandru Mihaileanu
!*  DATE                       : February 6, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NEGATIVE with complex numbers.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : The testcase follows the scenarios:
!*
!* 1.Test operation for negative reals of complex numbers
!* 2.Test operation for positive reals of complex numbers
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program neg_complex

        use ieee_arithmetic
         complex(4) :: cx
         complex(8) :: cx8
         complex(16) :: cx16


        logical :: flag_values(5)
        integer :: i

        ! ieee_is_negative should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)

! Test operation for negative reals of complex numbers

	cx = (-2345.89, -1.43E4)
        if (ieee_is_negative(real(cx)) .neqv. .true.) error stop 1 
        if (ieee_is_negative(imag(cx)) .neqv. .true.) error stop 2 

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)

        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 10
        end do


        call ieee_set_flag(ieee_all,.false.)

        cx8 = (-2345.89_8, -1.43D4)
        if (ieee_is_negative(real(cx8)) .neqv. .true.) error stop 3
        if (ieee_is_negative(imag(cx8)) .neqv. .true.) error stop 4 

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)

        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 20
        end do


        call ieee_set_flag(ieee_all,.false.)

        cx16 = (-2345.89_16, -1.43Q4)
        if (ieee_is_negative(real(cx16)) .neqv. .true.) error stop 5
        if (ieee_is_negative(imag(cx16)) .neqv. .true.) error stop 6

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)

        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 30
        end do



! Test operation for positive reals of complex numbers

        call ieee_set_flag(ieee_all,.false.)

        cx = (1.43E4, 23.98)
        if (ieee_is_negative(real(cx)) .neqv. .false.) error stop 7
        if (ieee_is_negative(imag(cx)) .neqv. .false.) error stop 8

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)

        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 40
        end do


        call ieee_set_flag(ieee_all,.false.)

        cx8 = (1.43D4, 999.9_8)
        if (ieee_is_negative(real(cx8)) .neqv. .false.) error stop 9
        if (ieee_is_negative(imag(cx8)) .neqv. .false.) error stop 11

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)

        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 50
        end do


        call ieee_set_flag(ieee_all,.false.)

        cx16 = (1.43Q4, 0.8976_16)
        if (ieee_is_negative(real(cx16)) .neqv. .false.) error stop 12
        if (ieee_is_negative(imag(cx16)) .neqv. .false.) error stop 13



        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)

        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 60 
        end do

        end program
