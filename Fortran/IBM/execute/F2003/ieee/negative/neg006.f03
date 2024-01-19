! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 6, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NEGATIVE with real array sections.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : IEEE_IS_NEGATIVE uses arguments as
!*                               array sections.One section contains
!*                               only positive elements and the other
!*                               section only negative.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program array_sections

        use ieee_arithmetic

        real(4) :: a(20)
        real(8) :: b(20)
        real(16) :: c(20)
        logical :: result4(20), result8(20), result16(20), flag_values(5)
        integer :: i

           a(1:20:2) = (/ (i,i=1,10,1) /)
           a(2:20:2) = (/ (i,i=-1,-10,-1) /)
           b(1:20:2) = (/ (i,i=10,100,10) /)
           b(2:20:2) = (/ (i,i=-10,-100,-10) /)
           c(1:20:2) = (/ (i,i=1000,10000,1000) /)
           c(2:20:2) = (/ (i,i=-1000,-10000,-1000) /)

        ! ieee_is_negative should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)


! Test real*4

	result4(1:20:2) = ieee_is_negative(a(1:20:2))
        do i = 1,10,2
        	if (result4(i).neqv. .false.) call zzrc(i)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+100)
        end do

        call ieee_set_flag(ieee_all,.false.)

        result4(2:20:2) = ieee_is_negative(a(2:20:2))
        do i = 2,10,2
        	if (result4(i).eqv. .false.) call zzrc(i)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+200)
        end do

! Test real*8

        call ieee_set_flag(ieee_all,.false.)

        result8(1:20:2) = ieee_is_negative(a(1:20:2))
        do i = 1,10,2
                if (result8(i).neqv. .false.) call zzrc(i+20)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+300)
        end do

        call ieee_set_flag(ieee_all,.false.)

        result8(2:20:2) = ieee_is_negative(a(2:20:2))
        do i = 2,10,2
                if (result8(i).eqv. .false.) call zzrc(i+20)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+400)
        end do

! Test real*16

        call ieee_set_flag(ieee_all,.false.)

        result16(1:20:2) = ieee_is_negative(a(1:20:2))
        do i = 1,10,2
                if (result16(i).neqv. .false.) call zzrc(i+40)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(i+500)
        end do

        call ieee_set_flag(ieee_all,.false.)

        result16(2:20:2) = ieee_is_negative(a(2:20:2))
        do i = 2,10,2
                if (result16(i).eqv. .false.) call zzrc(i+40)
        end do

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 77
        end do

        end program
