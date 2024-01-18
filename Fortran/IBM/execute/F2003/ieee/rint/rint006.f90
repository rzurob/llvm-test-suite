! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/fxieee.presh rint006
! %COMPOPTS: -qstrict -qfloat=rrm:nofold -qfree=f90 -qxlf90=signedzero
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
!*  DATE                       : March 13, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_RINT  w/ Infinities,Denormals and NaNs .
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_ROUNDING_MODE
!*                               IEEE_SET_ROUNDING_MODE
!*
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

         program rint_special

         use ieee_arithmetic
         use ieee_exceptions
         use constants_for_ieee

         implicit none
         integer :: i
         real*4 :: xr1, res1, xr2, res2
         real*8 :: xr1_8, res1_8, xr2_8, res2_8
         real*16 :: xr1_16, res1_16, xr2_16, res2_16
         integer*4 :: ires1, ires2
         integer*8 :: ires1_8, ires2_8
         integer*8 :: ires1_16(2), ires2_16(2)
         logical :: flag_values(5), expect_value(5)

         equivalence(ires1, res1)
         equivalence(ires2, res2)
         equivalence(ires1_8, res1_8)
         equivalence(ires2_8, res2_8)
         equivalence(ires1_16, res1_16)
         equivalence(ires2_16, res2_16)


         type(ieee_round_type) :: rtype
         type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
         type(ieee_round_type), parameter :: rt_20 = IEEE_TO_ZERO
         type(ieee_round_type), parameter :: rt_up = IEEE_UP
         type(ieee_round_type), parameter :: rt_down = IEEE_DOWN
         type(ieee_round_type), parameter :: rt_other = IEEE_OTHER
         type(ieee_status_type) :: status_value

!****************
!  test nereast
!****************
         call ieee_set_rounding_mode(rt_nearest)
         call ieee_get_rounding_mode(rtype)
         if (rtype /= rt_nearest)  error stop 1
         call ieee_get_status(status_value)

!  test real*4 infinities

            xr1 = PINF_4
            xr2 = NINF_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= PINF_4) error stop 2
            if (res2 /= NINF_4) error stop 3

!  test real*8 infinities

            xr1_8 = PINF_8
            xr2_8 = NINF_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= PINF_8) error stop 4
            if (res2_8 /= NINF_8) error stop 5

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 200
             enddo

!  test real*16 infinities

            xr1_16 = PINF_16
            xr2_16 = NINF_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= PINF_16) error stop 6
            if (res2_16 /= NINF_16) error stop 7

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 200
             enddo



!  test real*4 Denormals

         call ieee_set_status(status_value)

            xr1 = PHD_4
            xr2 = NHD_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= 0.0) error stop 8
            if (res2 /= 0.0) error stop 9


!  test real*8 Denormals

            xr1_8 = PHD_8
            xr2_8 = NHD_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= 0.0) error stop 10
            if (res2_8 /= 0.0) error stop 11

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 250
             enddo


!test real*16 Denormals

         call ieee_set_status(status_value)

            xr1_16 = tiny(1.0_16)/2.0_16
            xr2_16 = -tiny(1.0_16)/2.0_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= 0.0) error stop 10
            if (res2_16 /= 0.0) error stop 11

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 250
             enddo



!  test real*4 NaNQ

         call ieee_set_status(status_value)

            xr1 = PNANQ_4
            xr2 = NNANQ_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (ires1 /= iPNANQ_4) error stop 12
            if (ires2 /= iNNANQ_4) error stop 13

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 300
             enddo


!  test real*4 NaNS

         call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1 = PNANS_4
            xr2 = NNANS_4
            res1 = ieee_rint(xr1)

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (100+i)
        end do

         call ieee_set_status(status_value)

            res2 = ieee_rint(xr2)
            if (ires1 /= iPNANQ_4) error stop 14
            if (ires2 /= iNNANQ_4) error stop 15

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (110+i)
        end do

!  test real*8 NaNQ

         call ieee_set_status(status_value)

            xr1_8 = PNANQ_8
            xr2_8 = NNANQ_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= iPNANQ_8) error stop 16
            if (ires2_8 /= iNNANQ_8) error stop 17

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*8 NaNS

         call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1_8 = PNANS_8
            xr2_8 = NNANS_8
            res1_8 = ieee_rint(xr1_8)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (120+i)
        end do

             call ieee_set_status(status_value)

            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= iPNANQ_8) error stop 18
            if (ires2_8 /= iNNANQ_8) error stop 19
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (130+i)
        end do

!  test real*16 NaNQ

         call ieee_set_status(status_value)

            xr1_16 = PNANQ_16
            xr2_16 = NNANQ_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (ires1_16(1) /= iPNANQ_16(1)) error stop 20
            if (ires2_16(1) /= iNNANQ_16(1)) error stop 21

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*16 NaNS

             call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1_16 = PNANS_16
            xr2_16 = NNANS_16
            res1_16 = ieee_rint(xr1_16)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (140+i)
        end do

         call ieee_set_status(status_value)

            res2_16 = ieee_rint(xr2_16)
            if (ires1_16(1) /= iPNANQ_16(1)) error stop 22
            if (ires2_16(1) /= iNNANQ_16(1)) error stop 23
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (150+i)
        end do

!****************
! test  to zero
!****************
        call ieee_set_status(status_value)
        call ieee_set_rounding_mode(rt_20)
        call ieee_get_rounding_mode(rtype)
        if (rtype /= rt_20) error stop 24
        call ieee_get_status(status_value)

!  test real*4 infinities

            xr1 = PINF_4
            xr2 = NINF_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= PINF_4) error stop 25
            if (res2 /= NINF_4) error stop 26

!  test real*8 infinities

            xr1_8 = PINF_8
            xr2_8 = NINF_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= PINF_8) error stop 27
            if (res2_8 /= NINF_8) error stop 28

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*16 infinities

            xr1_16 = PINF_16
            xr2_16 = NINF_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= PINF_16) error stop 29
            if (res2_16 /= NINF_16) error stop 30

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*4 Denormals

         call ieee_set_status(status_value)

            xr1 = PHD_4
            xr2 = NHD_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= 0.0) error stop 31
            if (res2 /= 0.0) error stop 32

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo



!  test real*8 Denormals

         call ieee_set_status(status_value)

            xr1_8 = PHD_8
            xr2_8 = NHD_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= 0.0) error stop 33
            if (res2_8 /= 0.0) error stop 34

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!test real*16 Denormals

         call ieee_set_status(status_value)

            xr1_16 = tiny(1.0_16)/2.0_16
            xr2_16 = -tiny(1.0_16)/2.0_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= 0.0) error stop 10
            if (res2_16 /= 0.0) error stop 11

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 250
             enddo


!  test real*4 NaNQ

         call ieee_set_status(status_value)

            xr1 = PNANQ_4
            xr2 = NNANQ_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (ires1 /= iPNANQ_4) error stop 35
            if (ires2 /= iNNANQ_4) error stop 36

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*4 NaNS

         call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1 = PNANS_4
            xr2 = NNANS_4
            res1 = ieee_rint(xr1)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (160+i)
        end do

         call ieee_set_status(status_value)

            res2 = ieee_rint(xr2)
            if (ires1 /= iPNANQ_4) error stop 37
            if (ires2 /= iNNANQ_4) error stop 38
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (170+i)
        end do

!  test real*8 NaNQ

         call ieee_set_status(status_value)

            xr1_8 = PNANQ_8
            xr2_8 = NNANQ_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= iPNANQ_8) error stop 39
            if (ires2_8 /= iNNANQ_8) error stop 40

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*8 NaNS

         call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1_8 = PNANS_8
            xr2_8 = NNANS_8
            res1_8 = ieee_rint(xr1_8)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (180+i)
        end do

         call ieee_set_status(status_value)

            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= iPNANQ_8) error stop 41
            if (ires2_8 /= iNNANQ_8) error stop 42
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (190+i)
        end do

!  test real*16 NaNQ

         call ieee_set_status(status_value)

            xr1_16 = PNANQ_16
            xr2_16 = NNANQ_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (ires1_16(1) /= iPNANQ_16(1)) error stop 43
            if (ires2_16(1) /= iNNANQ_16(1)) error stop 44

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*16 NaNS

         call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1_16 = PNANS_16
            xr2_16 = NNANS_16
            res1_16 = ieee_rint(xr1_16)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (200+i)
        end do

         call ieee_set_status(status_value)

            res2_16 = ieee_rint(xr2_16)
            if (ires1_16(1) /= iPNANQ_16(1)) error stop 45
            if (ires2_16(1) /= iNNANQ_16(1)) error stop 46

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (210+i)
        end do

!****************
! test +INF
!****************

        call ieee_set_status(status_value)

        call ieee_set_rounding_mode(rt_up)
        call ieee_get_rounding_mode(rtype)
        if (rtype /= rt_up) error stop 47
        call ieee_get_status(status_value)


!  test real*4 infinities

            xr1 = PINF_4
            xr2 = NINF_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= PINF_4) error stop 48
            if (res2 /= NINF_4) error stop 49

!  test real*8 infinities

            xr1_8 = PINF_8
            xr2_8 = NINF_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= PINF_8) error stop 50
            if (res2_8 /= NINF_8) error stop 51

!  test real*16 infinities

            xr1_16 = PINF_16
            xr2_16 = NINF_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= PINF_16) error stop 52
            if (res2_16 /= NINF_16) error stop 53

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*4 Denormals

         call ieee_set_status(status_value)

            xr1 = PHD_4
            xr2 = NHD_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= 1.0) error stop 54
            if (res2 /= 0.0) error stop 55


           ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*8 Denormals

         call ieee_set_status(status_value)

            xr1_8 = PHD_8
            xr2_8 = NHD_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= 1.0) error stop 56
            if (res2_8 /= 0.0) error stop 57


             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo

!test real*16 Denormals

         call ieee_set_status(status_value)

            xr1_16 = tiny(1.0_16)/2.0_16
            xr2_16 = -tiny(1.0_16)/2.0_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= 1.0) error stop 10
            if (res2_16 /= 0.0) error stop 11

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 250
             enddo



!  test real*4 NaNQ

         call ieee_set_status(status_value)

            xr1 = PNANQ_4
            xr2 = NNANQ_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (ires1 /= iPNANQ_4) error stop 58
            if (ires2 /= iNNANQ_4) error stop 59

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*4 NaNS

         call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1 = PNANS_4
            xr2 = NNANS_4
            res1 = ieee_rint(xr1)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (220+i)
        end do

         call ieee_set_status(status_value)

            res2 = ieee_rint(xr2)
            if (ires1 /= iPNANQ_4) error stop 60
            if (ires2 /= iNNANQ_4) error stop 61
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (230+i)
        end do

!  test real*8 NaNQ

         call ieee_set_status(status_value)

            xr1_8 = PNANQ_8
            xr2_8 = NNANQ_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= iPNANQ_8) error stop 62
            if (ires2_8 /= iNNANQ_8) error stop 63

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*8 NaNS

         call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1_8 = PNANS_8
            xr2_8 = NNANS_8
            res1_8 = ieee_rint(xr1_8)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (240+i)
        end do

         call ieee_set_status(status_value)

            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= iPNANQ_8) error stop 64
            if (ires2_8 /= iNNANQ_8) error stop 65

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (250+i)
        end do

             call ieee_set_status(status_value)

!  test real*16 NaNQ

            xr1_16 = PNANQ_16
            xr2_16 = NNANQ_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (ires1_16(1) /= iPNANQ_16(1)) error stop 66
            if (ires2_16(1) /= iNNANQ_16(1)) error stop 67

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*16 NaNS

         call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1_16 = PNANS_16
            xr2_16 = NNANS_16
            res1_16 = ieee_rint(xr1_16)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (260+i)
        end do

             call ieee_set_status(status_value)

            res2_16 = ieee_rint(xr2_16)
            if (ires1_16(1) /= iPNANQ_16(1)) error stop 68
            if (ires2_16(1) /= iNNANQ_16(1)) error stop 69

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (270+i)
        end do

!****************
! test to -INF
!****************

         call ieee_set_status(status_value)

         call ieee_set_rounding_mode(rt_down)
         call ieee_get_rounding_mode(rtype)
         if (rtype /= rt_down) error stop 70
         call ieee_get_status(status_value)

!  test real*4 infinities

            xr1 = PINF_4
            xr2 = NINF_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= PINF_4) error stop 71
            if (res2 /= NINF_4) error stop 72

!  test real*8 infinities

             call ieee_set_status(status_value)

            xr1_8 = PINF_8
            xr2_8 = NINF_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= PINF_8) error stop 73
            if (res2_8 /= NINF_8) error stop 74

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*16 infinities

            xr1_16 = PINF_16
            xr2_16 = NINF_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= PINF_16) error stop 75
            if (res2_16 /= NINF_16) error stop 76

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*4 Denormals

             call ieee_set_status(status_value)

            xr1 = PHD_4
            xr2 = NHD_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (res1 /= 0.0) error stop 77
            if (res2 /= -1.0) error stop 78

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo



!  test real*8 Denormals

             call ieee_set_status(status_value)

            xr1_8 = PHD_8
            xr2_8 = NHD_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (res1_8 /= 0.0) error stop 79
            if (res2_8 /= -1.0) error stop 80


             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo

!test real*16 Denormals

         call ieee_set_status(status_value)

            xr1_16 = tiny(1.0_16)/2.0_16
            xr2_16 = -tiny(1.0_16)/2.0_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (res1_16 /= 0.0) error stop 10
            if (res2_16 /= -1.0) error stop 11

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 250
             enddo


!  test real*4 NaNQ

             call ieee_set_status(status_value)

            xr1 = PNANQ_4
            xr2 = NNANQ_4
            res1 = ieee_rint(xr1)
            res2 = ieee_rint(xr2)
            if (ires1 /= iPNANQ_4) error stop 81
            if (ires2 /= iNNANQ_4) error stop 82

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*4 NaNS

             call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1 = PNANS_4
            xr2 = NNANS_4
            res1 = ieee_rint(xr1)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (280+i)
        end do

             call ieee_set_status(status_value)

            res2 = ieee_rint(xr2)
            if (ires1 /= iPNANQ_4) error stop 83
            if (ires2 /= iNNANQ_4) error stop 84
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (290+i)
        end do

!  test real*8 NaNQ

             call ieee_set_status(status_value)

            xr1_8 = PNANQ_8
            xr2_8 = NNANQ_8
            res1_8 = ieee_rint(xr1_8)
            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= iPNANQ_8) error stop 85
            if (ires2_8 /= iNNANQ_8) error stop 86

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*8 NaNS

             call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1_8 = PNANS_8
            xr2_8 = NNANS_8
            res1_8 = ieee_rint(xr1_8)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (300+i)
        end do

             call ieee_set_status(status_value)

            res2_8 = ieee_rint(xr2_8)
            if (ires1_8 /= iPNANQ_8) error stop 87
            if (ires2_8 /= iNNANQ_8) error stop 88
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (310+i)
        end do

!  test real*16 NaNQ

             call ieee_set_status(status_value)

            xr1_16 = PNANQ_16
            xr2_16 = NNANQ_16
            res1_16 = ieee_rint(xr1_16)
            res2_16 = ieee_rint(xr2_16)
            if (ires1_16(1) /= iPNANQ_16(1)) error stop 89
            if (ires2_16(1) /= iNNANQ_16(1)) error stop 90

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


!  test real*16 NaNS

             call ieee_set_status(status_value)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

            xr1_16 = PNANS_16
            xr2_16 = NNANS_16
            res1_16 = ieee_rint(xr1_16)
        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (320+i)
        end do

             call ieee_set_status(status_value)

            res2_16 = ieee_rint(xr2_16)
            if (ires1_16(1) /= iPNANQ_16(1)) error stop 91
            if (ires2_16(1) /= iNNANQ_16(1)) error stop 92

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,4
           if (expect_value(i).neqv.flag_values(i)) call zzrc (330+i)
        end do

         end program
