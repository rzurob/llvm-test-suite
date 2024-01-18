! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh neg002
! %COMPOPTS: -qstrict -qfloat=nomaf -qfree=f90 -qxlf90=signedzero
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NEGATIVE with Infinities,
!*                               NaNs, and Denormals.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : This testcase follows the scenarios :
!*
!* 1.Test +/- infinity for real*4
!* 2.Test  NaNS/NaNQ for real*4
!* 3.Test +/- infinity for real*8
!* 4.Test  NaNS/NaNQ for real*8
!* 5.Test +/- infinity for real*16
!* 6.Test  NaNS/NaNQ for real*16
!* 7.Test Denormals for real*4/real*8
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program neg_special

        use ieee_arithmetic
	use constants_for_ieee

        real*4 :: var_4
        real*8 :: var_8
        real*16 :: var_16
        logical :: result, flag_values(5)
        integer :: i

        ! ieee_is_negative should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)

! Test positive infinity for real*4

        call ieee_set_flag(ieee_all,.false.)

	var_4 = X'7F800000'
	result = ieee_is_negative(var_4)
	if (result .neqv. .false.) error stop 1

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 10
        enddo

! Test negative infinity for real*4

        call ieee_set_flag(ieee_all,.false.)


        var_4 = X'FF800000'
        result = ieee_is_negative(var_4)
        if (result .eqv. .false.) error stop 2

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 20
        enddo

! Test  NaNS for real*4. The function should return false.

        call ieee_set_flag(ieee_all,.false.)


        var_4 = X'7F800001'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 3

        var_4 = X'7F800002'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 4

        var_4 = X'7FBFFFFF'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 5

        var_4 = X'FF800001'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 6

        var_4 = X'FF800002'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 7

        var_4 = X'FFBFFFFF'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 8

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 30
        enddo


! Test  NaNQ for real*4. The function should return false.

        call ieee_set_flag(ieee_all,.false.)


        var_4 = X'7FC00000'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 9

        var_4 = X'7FC00001'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 10

        var_4 = X'7FFFFFFF'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 11

        var_4 = X'FFC00000'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 12

        var_4 = X'FFC00001'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 13

        var_4 = X'FFFFFFFF'
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 14

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 40
        enddo

! Test positive infinity for real*8.

        call ieee_set_flag(ieee_all,.false.)


        var_8 = X'7FF0000000000000'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 15

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 50
        enddo


! Test negative infinity for real*8.

        call ieee_set_flag(ieee_all,.false.)


        var_8 = X'FFF0000000000000'
        result = ieee_is_negative(var_8)
        if (result .eqv. .false.) error stop 16

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 60
        enddo

! Test  NaNS for real*8. The function should return false.

        call ieee_set_flag(ieee_all,.false.)


        var_8 = X'7FF0000000000001'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 17

        var_8 = X'7FF0000000000002'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 18

        var_8 = X'7FF7FFFFFFFFFFFF'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 19

        var_8 = X'FFF0000000000001'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 21

        var_8 = X'FFF0000000000002'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 22

        var_8 = X'FFF7FFFFFFFFFFFF'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 23

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 70
        enddo

! Test  NaNQ for real*8. The function should return false.

        call ieee_set_flag(ieee_all,.false.)


        var_8 = X'7FF8000000000000'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 24

        var_8 = X'7FF8000000000001'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 25

        var_8 = X'7FFFFFFFFFFFFFFF'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 26

        var_8 = X'FFF8000000000000'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 27

        var_8 = X'FFF8000000000001'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 28

        var_8 = X'FFFFFFFFFFFFFFFF'
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 29

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 80
        enddo

! Test positive infinity for real*16.

        call ieee_set_flag(ieee_all,.false.)


        var_16 = X'7FF00000000000000000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 31

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 90
        enddo


! Test negative infinity for real*16.

        call ieee_set_flag(ieee_all,.false.)


        var_16 = X'FFF00000000000000000000000000000'
        result = ieee_is_negative(var_16)
        if (result .eqv. .false.) error stop 32

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 100
        enddo


! Test  NaNS for real*16. The function should return false.

        call ieee_set_flag(ieee_all,.false.)


        var_16 = X'7FF00000000000010000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 33

        var_16 = X'7FF00000000000020000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 34

        var_16 = X'7FF7FFFFFFFFFFFF0000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 35

        var_16 = X'FFF00000000000010000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 36

        var_16 = X'FFF00000000000020000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 37

        var_16 = X'FFF7FFFFFFFFFFFF0000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 38

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 110
        enddo


! Test  NaNQ for real*16. The function should return false.

        call ieee_set_flag(ieee_all,.false.)


        var_16 = X'7FF80000000000000000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 39

        var_16 = X'7FF80000000000010000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 41

        var_16 = X'7FFFFFFFFFFFFFFF0000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 42

        var_16 = X'FFF80000000000000000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 43

        var_16 = X'FFF80000000000010000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 44

        var_16 = X'FFFFFFFFFFFFFFFF0000000000000000'
        result = ieee_is_negative(var_16)
        if (result .neqv. .false.) error stop 45


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 120
        enddo

! Test Denormals for real*4

        call ieee_set_flag(ieee_all,.false.)

        var_4 = PTD_4
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 46

        var_4 = PHD_4
        result = ieee_is_negative(var_4)
        if (result .neqv. .false.) error stop 47


        var_4 = NTD_4
        result = ieee_is_negative(var_4)
        if (result .neqv. .true.) error stop 48

        var_4 = NHD_4
        result = ieee_is_negative(var_4)
        if (result .neqv. .true.) error stop 49

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 130
        enddo


! Test Denormals for real*8

        call ieee_set_flag(ieee_all,.false.)

        var_8 = PTD_8
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 51

        var_8 = PHD_8
        result = ieee_is_negative(var_8)
        if (result .neqv. .false.) error stop 52


        var_8 = NTD_8
        result = ieee_is_negative(var_8)
        if (result .neqv. .true.) error stop 53

        var_8 = NHD_8
        result = ieee_is_negative(var_8)
        if (result .neqv. .true.) error stop 54

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 140
        enddo



        end program
