! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh value003
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
!*  aL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Alexandru Mihaileanu
!*  DATE                       : February 12, 2002
!*  ORIGIN                     : aL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_VALUE with constants.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
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

	program value_val

        use ieee_arithmetic
        use constants_for_ieee

        real*4 :: a
        type(ieee_class_type), parameter :: Y1 = ieee_signaling_nan
        type(ieee_class_type), parameter :: Y2 = ieee_quiet_nan
        type(ieee_class_type), parameter :: Y3 = ieee_negative_inf
        type(ieee_class_type), parameter :: Y4 = ieee_positive_inf
        type(ieee_class_type), parameter :: Y5 = ieee_negative_normal
        type(ieee_class_type), parameter :: Y6 = ieee_positive_normal
        type(ieee_class_type), parameter :: Y7 = ieee_negative_denormal
        type(ieee_class_type), parameter :: Y8 = ieee_positive_denormal
        type(ieee_class_type), parameter :: Y9 = ieee_negative_zero
        type(ieee_class_type), parameter :: Y10 = ieee_positive_zero
        real*8 :: b
	real*16 :: c
        logical, dimension(5) :: flag_values
        integer :: i

!       ieee_value should not set any flags.  Clear all flags and
!       check at the end that all flags are clear.
        call ieee_set_flag(ieee_all,.false.)

!***********************
!       Test real*4
!***********************

        a = ieee_value(a, Y1)
        if (ieee_class(a) /= Y1) error stop 1 

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(100+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        a = ieee_value(a, Y2)
        if (ieee_class(a) /= Y2) error stop 2

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(110+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        a = ieee_value(a, Y3)
        if (ieee_class(a) /= Y3) error stop 3

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(120+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        a = ieee_value(a, Y4)
        if (ieee_class(a) /= Y4) error stop 4

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(130+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)



        a = ieee_value(a, Y5)
        if (ieee_class(a) /= Y5) error stop 5

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(140+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        a = ieee_value(a, Y6)
        if (ieee_class(a) /= Y6) error stop 6

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(150+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)



        a = ieee_value(a, Y7)
        if (ieee_class(a) /= Y7) error stop 7

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)



        a = ieee_value(a, Y8)
        if (ieee_class(a) /= Y8) error stop 8

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)



        a = ieee_value(a, Y9)
        if (ieee_class(a) /= Y9) error stop 9

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(170+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        a = ieee_value(a, Y10)
        if (ieee_class(a) /= Y10) error stop 10

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(180+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)



!***********************
!       Test real*8
!***********************


        b = ieee_value(b, Y1)
        if (ieee_class(b) /= Y1) error stop 11 

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(100+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        b = ieee_value(b, Y2)
        if (ieee_class(b) /= Y2) error stop 12

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(110+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        b = ieee_value(b, Y3)
        if (ieee_class(b) /= Y3) error stop 13

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(120+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        b = ieee_value(b, Y4)
        if (ieee_class(b) /= Y4) error stop 14

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(130+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        b = ieee_value(b, Y5)
        if (ieee_class(b) /= Y5) error stop 15

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(140+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        b = ieee_value(b, Y6)
        if (ieee_class(b) /= Y6) error stop 16

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(150+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        b = ieee_value(b, Y7)
        if (ieee_class(b) /= Y7) error stop 17

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        b = ieee_value(b, Y8)
        if (ieee_class(b) /= Y8) error stop 18

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        b = ieee_value(b, Y9)
        if (ieee_class(b) /= Y9) error stop 19

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(170+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        b = ieee_value(b, Y10)
        if (ieee_class(b) /= Y10) error stop 20

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(180+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)

!***********************
!       Test real*16
!***********************

        c = ieee_value(c, Y1)
        if (ieee_class(c) /= Y1) error stop 21 

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(100+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        c = ieee_value(c, Y2)
        if (ieee_class(c) /= Y2) error stop 22

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(110+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        c = ieee_value(c, Y3)
        if (ieee_class(c) /= Y3) error stop 23

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(120+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        c = ieee_value(c, Y4)
        if (ieee_class(c) /= Y4) error stop 24

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(130+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        c = ieee_value(c, Y5)
        if (ieee_class(c) /= Y5) error stop 25

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(140+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        c = ieee_value(c, Y6)
        if (ieee_class(c) /= Y6) error stop 26

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(150+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        c = ieee_value(c, Y7)
        if (ieee_class(c) /= Y7) error stop 27

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        c = ieee_value(c, Y8)
        if (ieee_class(c) /= Y8) error stop 28

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(160+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        c = ieee_value(c, Y9)
        if (ieee_class(c) /= Y9) error stop 29

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(170+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        c = ieee_value(c, Y10)
        if (ieee_class(c) /= Y10) error stop 30

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)call zzrc(180+i)
        enddo

        call ieee_set_flag(ieee_all,.false.)


        end program

