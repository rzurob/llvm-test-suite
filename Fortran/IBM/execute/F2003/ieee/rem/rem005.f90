! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh rem005
! %COMPOPTS: -qfree=f90 -qstrict
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
!*  DATE                       : March 14, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_REM(X,Y)= X-Y*N
!*                               different rounding types of N 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Function Result = X - Y * N
!*                               N is a rounding of n 
!*                               where n = X / Y
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program raport_value

        use ieee_arithmetic
        use constants_for_ieee
	implicit none

        real*4 :: x4, y4, z4
        real*8 :: x8, y8, z8
	integer*4 :: i, ires4, ires8, ires16
        logical, dimension(5) :: flag_values, expect_value

	equivalence(z4,ires4)
	equivalence(z8,ires8)

!       Test real*4 /real*8  for  n = +0, N = +0

        call ieee_set_flag(ieee_all,.false.)

        x4 = 0.0
        y4 = 3.0
        x8 = 0.0_8
	y8 = 900.9_8

        z4 = ieee_rem(x4, y4)
	if (ires4 /= z"00000000" .and. z4 /= 0.0) error stop 1

        z8 = ieee_rem(x8, y8)
	if (ires8 /= z"00000000" .and. z8 /= 0.0) error stop 2

        ! Now check that no flags were turned on, other than inexact.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(60+i)
        enddo


!       Test real*4 /real*8  for  n = -0, N = -0

        call ieee_set_flag(ieee_all,.false.)

        x4 = -0.0
        y4 = 3.0
        x8 = -0.0_8
        y8 = 900.9_8

        z4 = ieee_rem(x4, y4)
        if (ires4 /= z"80000000".and. z4 /= 0.0) error stop 4

        z8 = ieee_rem(x8, y8)
        if (ires8 /= z"80000000".and. z8 /= 0.0) error stop 5

        ! Now check that no flags were turned on, other than inexact.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(65+i)
        enddo

!       Test real*4 /real*8  for n = -0, N = -0

        call ieee_set_flag(ieee_all,.false.)

        x4 = 0.0
        y4 = -3.0
        x8 = 0.0_8
        y8 = -900.9_8

        z4 = ieee_rem(x4, y4)
        if (ires4 /= z"00000000".and. z4 /= 0.0) error stop 7

        z8 = ieee_rem(x8, y8)
        if (ires8 /= z"00000000".and. z8 /= 0.0) error stop 8

        ! Now check that no flags were turned on, other than inexact.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(70+i)
        enddo


!       Test real*4 /real*8  for n = 0.5 , N = +0

        call ieee_set_flag(ieee_all,.false.)

        x4 = 5.0
        y4 = 10.0
        x8 = 5.0_8
        y8 = 10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        ! Now check that no flags were turned on, other than inexact.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(75+i)
        enddo

        if (z4 /= 5.0 ) error stop 10
        if (z8 /= 5.0_8 ) error stop 11

!       Test real*4 /real*8  for n = 0.5, N = +0

        call ieee_set_flag(ieee_all,.false.)

        x4 = -5.0
        y4 = -10.0
        x8 = -5.0_8
        y8 = -10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        ! Now check that no flags were turned on, other than inexact.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(80+i)
        enddo
        if (z4 /= -5.0 ) error stop 13
        if (z8 /= -5.0_8 ) error stop 14

!       Test real*4 /real*8  for n = -0.5, N = -0

        call ieee_set_flag(ieee_all,.false.)

        x4 = -5.0
        y4 = 10.0
        x8 = -5.0_8
        y8 = 10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        ! Now check that no flags were turned on, other than inexact.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(85+i)
        enddo

        if (z4 /= -5.0 ) error stop 16
        if (z8 /= -5.0_8 ) error stop 17

!       Test real*4 /real*8   for n = -0.5, N = -0

        call ieee_set_flag(ieee_all,.false.)

        x4 = 5.0
        y4 = -10.0
        x8 = 5.0_8
        y8 = -10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        ! Now check that no flags were turned on, other than inexact.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(90+i)
        enddo

        if (z4 /= 5.0 ) error stop 19
        if (z8 /= 5.0_8 ) error stop 20

!       Test real*4 /real*8  for n = 0.7, N = 1

        call ieee_set_flag(ieee_all,.false.)
        
        x4 = 7.0
        y4 = 10.0
        x8 = 7.0_8
        y8 = 10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        ! Now check that no flags were turned on, other than inexact.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(100+i)
        enddo

        if (z4 /= -3.0 ) error stop 22
        if (z8 /= -3.0_8 ) error stop 23

!       Test real*4 /real*8   for n = 0.7, N = 1

        call ieee_set_flag(ieee_all,.false.)

        x4 = -7.0
        y4 = -10.0
        x8 = -7.0_8
        y8 = -10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        ! Now check that no flags were turned on, other than inexact.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(105+i)
        enddo

        if (z4 /= 3.0 ) error stop 25
        if (z8 /= 3.0_8 ) error stop 26

!       Test real*4 /real*8   for n = -0.7, N = -1

        call ieee_set_flag(ieee_all,.false.)

        x4 = -7.0
        y4 = 10.0
        x8 = -7.0_8
        y8 = 10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        ! Now check that no flags were turned on, other than inexact.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(110+i)
        enddo


        if (z4 /= 3.0 ) error stop 28
        if (z8 /= 3.0_8 ) error stop 29

!       Test real*4 /real*8  for n = -0.7, N = -1

        call ieee_set_flag(ieee_all,.false.)



        x4 = 7.0
        y4 = -10.0
        x8 = 7.0_8
        y8 = -10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(115+i)
        enddo


        if (z4 /= -3.0 ) error stop 31
        if (z8 /= -3.0_8 ) error stop 32

!       Test real*4 /real*8 for n = 1.49, N = 1

        call ieee_set_flag(ieee_all,.false.)



        x4 = 149.0
        y4 = 100.0
        x8 = 149.0_8
        y8 = 100.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(120+i)
        enddo


        if (z4 /= 49.0 ) error stop 34
        if (z8 /= 49.0_8 ) error stop 35

!       Test real*4 /real*8   for n = -1.49, N = -1

        call ieee_set_flag(ieee_all,.false.)



        x4 = 149.0
        y4 = -100.0
        x8 = 149.0_8
        y8 = -100.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(125+i)
        enddo

        if (z4 /= 49.0 ) error stop 37
        if (z8 /= 49.0_8 ) error stop 38

!       Test real*4 /real*8   for n = 1.79, N = 2

        call ieee_set_flag(ieee_all,.false.)

        x4 = 179.0
        y4 = 100.0
        x8 = 179.0_8
        y8 = 100.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(130+i)
        enddo

        if (z4 /= -21.0 ) error stop 40
        if (z8 /= -21.0_8 ) error stop 41

!       Test real*4 /real*8   for n = -1.79, N = -2

        call ieee_set_flag(ieee_all,.false.)


        x4 = 179.0
        y4 = -100.0
        x8 = 179.0_8
        y8 = -100.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(135+i)
        enddo


        if (z4 /= -21.0 ) error stop 43
        if (z8 /= -21.0_8 ) error stop 44
 
!       Test real*4 /real*8   for n = 4.5 , N = 4

        call ieee_set_flag(ieee_all,.false.)


        x4 = 45.0
        y4 = 10.0
        x8 = 45.0_8
        y8 = 10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(140+i)
        enddo

        if (z4 /=  5.0 ) error stop 46
        if (z8 /=  5.0_8 ) error stop 47
 
!       Test real*4 /real*8   for n = -4.5 , N = -4

        call ieee_set_flag(ieee_all,.false.)



        x4 = -45.0
        y4 = 10.0
        x8 = -45.0_8
        y8 = 10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(145+i)
        enddo


        if (z4 /= -5.0 ) error stop 49
        if (z8 /= -5.0_8 ) error stop 50

!       Test real*4 /real*8 / real*16  for n = 5.5 , N = 6

        call ieee_set_flag(ieee_all,.false.)



        x4 = 55.0
        y4 = 10.0
        x8 = 55.0_8
        y8 = 10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(150+i)
        enddo

        if (z4 /= -5.0 ) error stop 52
        if (z8 /= -5.0_8 ) error stop 53

!       Test real*4 /real*8   for n = -5.5 , N = -6

        call ieee_set_flag(ieee_all,.false.)



        x4 = 55.0
        y4 = -10.0
        x8 = 55.0_8
        y8 = -10.0_8
        z4 = ieee_rem(x4, y4)
        z8 = ieee_rem(x8, y8)

        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,4
            if (flag_values(i) .neqv. .false.)call zzrc(155+i)
        enddo


        if (z4 /= -5.0 ) error stop 55
        if (z8 /=  -5.0_8 ) error stop 56
		     
        end program

