! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh neg005 
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NEGATIVE with internal procedures.
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
!*  DESCRIPTION                : This testcase tests that values are passed
!*                               correctly to an internal procedure.
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

     program neg_intpro

     use ieee_arithmetic

        real(4) :: a
        real(8) :: b
        real(16) :: c
        logical :: results(4), flag_values(5)
        integer :: i

        ! ieee_is_negative should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)

! Test operation for negative reals

        a = -1.59E4
        b = -987654321.0_8
        c = 0.0_16

      call bar1(a)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+10)
        end do


        call ieee_set_flag(ieee_all,.false.)

      call bar2(b)

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+20)
        end do


        call ieee_set_flag(ieee_all,.false.)

      call bar3(c)


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+30)
        end do

        contains

        subroutine bar1(x4)
        use ieee_arithmetic
        real(4) :: x4
        if (ieee_is_negative(x4) .neqv. .true.) error stop 1
        end subroutine bar1

        subroutine bar2(x8)
        use ieee_arithmetic
        real(8) :: x8
        if (ieee_is_negative(x8) .neqv. .true.) error stop 2
        end subroutine bar2

        subroutine bar3(x16)
        use ieee_arithmetic
        real(16) :: x16
        if (ieee_is_negative(x16) .neqv. .false.) error stop 3
        end subroutine bar3


        end 
