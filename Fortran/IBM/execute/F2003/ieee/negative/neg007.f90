! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh neg007
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
!*
!*  DATE                       : February 6, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NEGATIVE with arrays of reals.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : IEEE_IS_NEGATIVE has real arrays as
!*                               arguments.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program arrays

        use ieee_arithmetic

        real(4) :: a1(20), a2(20)
        real(8) :: b1(20), b2(20)
        real(16) :: c1(20), c2(20)
        logical :: result4(20), result8(20), result16(20), flag_values(5)
        integer :: i


           a1 = (/ (i+0.2345,i=1,20,1) /)
           a2 = (/ (i-0.2345,i=-1,-20,-1) /)
           b1 = (/ (i+0987654,i=10,200,10) /)
           b2 = (/ (i-0987654,i=-10,-200,-10) /)
           c1 = (/ (i+0.901901,i=1000,20000,1000) /)
           c2 = (/ (i+0.901901,i=-1000,-20000,-1000) /)


        ! ieee_is_negative should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)


! Test real*4

	result4 = ieee_is_negative(a1)
        do i = 1,20
           if (result4(i).neqv. .false.) call zzrc(i)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+200)
        end do

        call ieee_set_flag(ieee_all,.false.)

        result4 = ieee_is_negative(a2)
        do i = 1,20
           if (result4(i).eqv. .false.) call zzrc(i+20)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+300)
        end do


! Test real*8

        call ieee_set_flag(ieee_all,.false.)

        result8 = ieee_is_negative(b1)
        do i = 1,20
           if (result8(i).neqv. .false.) call zzrc(i+40)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+400)
        end do

        call ieee_set_flag(ieee_all,.false.)

        result8 = ieee_is_negative(b2)
        do i = 1,20
           if (result8(i).eqv. .false.) call zzrc(i+60)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+500)
        end do

! Test real*16

        call ieee_set_flag(ieee_all,.false.)

        result16 = ieee_is_negative(c1)
        do i = 1,20
           if (result16(i).neqv. .false.) call zzrc(i+80)
        end do
        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+600)
        end do

        call ieee_set_flag(ieee_all,.false.)

        result16 = ieee_is_negative(c2)
        do i = 1,20
           if (result16(i).eqv. .false.) call zzrc(i+100)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+700)
        end do

        end program
