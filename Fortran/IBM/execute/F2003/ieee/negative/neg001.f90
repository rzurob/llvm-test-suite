! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh neg001 
! %COMPOPTS: -qfree=f90 -qstrict -qxlf90=signedzero
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NEGATIVE with reals.
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
!*  DESCRIPTION                : This testcase tests as follows :
!*
!* 1.operation for negative reals
!* 2.operation for inegative denormals
!* 3.operation for -INF
!* 4.operation for pozitive reals
!* 5.operation for pozitive denormals
!* 6.operation for +INF
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program neg_reals

        use ieee_arithmetic

        real(4) :: a
        real(8) :: b
        real(16) :: c
        logical :: flag_values(5)
        integer :: i

        ! ieee_is_negative should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)

! Test operation for negative reals

        a = -987654321.0
 	b = -987654321.0_8
        c = -987654321.0_16

        if (ieee_is_negative(a) .neqv. .true.) error stop 1 
        if (ieee_is_negative(b) .neqv. .true.) error stop 2 
        if (ieee_is_negative(c) .neqv. .true.) error stop 3 

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 10
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = -0.98765
        b = -0.98765_8
        c = -0.98765_16

        if (ieee_is_negative(a) .neqv. .true.) error stop 4
        if (ieee_is_negative(b) .neqv. .true.) error stop 5
        if (ieee_is_negative(c) .neqv. .true.) error stop 6

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 20
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = -0.0
        b = -0.0_8
        c = -0.0_16

        if (ieee_is_negative(a) .neqv. .true.) error stop 7
        if (ieee_is_negative(b) .neqv. .true.) error stop 8
        if (ieee_is_negative(c) .neqv. .true.) error stop 9

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 30
        enddo


!Test denormals
        call ieee_set_flag(ieee_all,.false.)

        a = z"807fffff"
        b = x"800fffffffffffff"
        c = -tiny(1.0)/ 2.0

        if (ieee_is_negative(a) .neqv. .true.) error stop 9
        if (ieee_is_negative(b) .neqv. .true.) error stop 10
        if (ieee_is_negative(c) .neqv. .true.) error stop 11

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 40
        enddo


! Test -INF
        call ieee_set_flag(ieee_all,.false.)

        a = z"ff800000"
        b = z"fff0000000000000"
        c = z"fff00000000000000000000000000000"

        if (ieee_is_negative(a) .neqv. .true.) error stop 12
        if (ieee_is_negative(b) .neqv. .true.) error stop 13
        if (ieee_is_negative(c) .neqv. .true.) error stop 14


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 50
        enddo


! Test for pozitive reals
        call ieee_set_flag(ieee_all,.false.)

        a = 0.0 
        b = 0.0_8
        c = 0.0_16

        if (ieee_is_negative(a) .neqv. .false.) error stop 15
        if (ieee_is_negative(b) .neqv. .false.) error stop 16
        if (ieee_is_negative(c) .neqv. .false.) error stop 17

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 60
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = 0.98765
        b = 0.98765_8
        c = 0.98765_16

        if (ieee_is_negative(a) .neqv. .false.) error stop 18
        if (ieee_is_negative(b) .neqv. .false.) error stop 19
        if (ieee_is_negative(c) .neqv. .false.) error stop 20

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 70
        enddo

        call ieee_set_flag(ieee_all,.false.)

        a = 987654321.0
        b = 987654321.0_8
        c = 987654321.0_16

        if (ieee_is_negative(a) .neqv. .false.) error stop 21
        if (ieee_is_negative(b) .neqv. .false.) error stop 22
        if (ieee_is_negative(c) .neqv. .false.) error stop 23

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 80
        enddo


! Test denormals
        call ieee_set_flag(ieee_all,.false.)

        a = z"007fffff"
        b = z"000fffffffffffff"
        c = tiny(1.0)/2.0

        if (ieee_is_negative(a) .neqv. .false.) error stop 24
        if (ieee_is_negative(b) .neqv. .false.) error stop 25
        if (ieee_is_negative(c) .neqv. .false.) error stop 26

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 90
        enddo


! Test +INF
        call ieee_set_flag(ieee_all,.false.)

        a = z"7f800000"
        b = z"7ff0000000000000"
        c = z"7ff00000000000000000000000000000"

        if (ieee_is_negative(a) .neqv. .false.) error stop 27
        if (ieee_is_negative(b) .neqv. .false.) error stop 28
        if (ieee_is_negative(c) .neqv. .false.) error stop 29

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 100 
        enddo


        end program
