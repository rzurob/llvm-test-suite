! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 5, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_LOGB with real Infinities and NaNs.
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
!* 1.Test +/- NANs for real*4/*8/*16
!* 2.Test +/- INF for real*4/*8/*16
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        include 'ieeeconsts.h'

	program logb_inf_nan

        use ieee_arithmetic
        use constants_for_ieee

	integer :: i, iy_4
	integer*8 :: iy_8, iy_16(2)
        real*4 :: x_4, y_4
        real*8 :: x_8, y_8
        real*16 :: x_16, y_16
        logical :: flag_value, expect_value(5),flag_values(5)
        type(ieee_status_type) :: status_value

        equivalence(iy_4,y_4)
        equivalence(iy_8,y_8)
        equivalence(iy_16,y_16)

!Test  NANs

        call ieee_set_flag(ieee_all,.false.)

        x_4 = PNANQ_4
        y_4 = ieee_logb(x_4)
        if (iy_4 /= iPNANQ_4)error stop 1
                                         !ieee_logb failed real*4 w/ PNANQ

        x_4 = NNANQ_4
        y_4 = ieee_logb(x_4)
        if (iy_4 /= iNNANQ_4)error stop 2
                                         !ieee_logb failed real*4 w/ NNANQ


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 20
        enddo


        call ieee_set_flag(ieee_all,.false.)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

        x_4 = PNANS_4
        y_4 = ieee_logb(x_4)
        if (iy_4 /= iPNANQ_4)error stop 3
                                         !ieee_logb failed real*4 w/ PNANS

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (60+i)
        end do


        call ieee_set_flag(ieee_all,.false.)


        x_4 = NNANS_4
        y_4 = ieee_logb(x_4)
        if (iy_4 /= iNNANQ_4)error stop 4
                                         !ieee_logb failed real*4 w/ NNANS

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (70+i)
        end do



        call ieee_set_flag(ieee_all,.false.)

        x_8 = PNANQ_8
        y_8 = ieee_logb(x_8)
        if (iy_8 /= iPNANQ_8)error stop 5
                                         !ieee_logb failed real*8 w/ PNANQ

        x_8 = NNANQ_8
        y_8 = ieee_logb(x_8)
        if (iy_8 /= iNNANQ_8)error stop 6
                                         !ieee_logb failed real*8 w/ NNANQ


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 30
        enddo


        call ieee_set_flag(ieee_all,.false.)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

        x_8 = PNANS_8
        y_8 = ieee_logb(x_8)
        if (iy_8 /= iPNANQ_8)error stop 7
                                         !ieee_logb failed real*8 w/ PNANS

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (80+i)
        end do


        call ieee_set_flag(ieee_all,.false.)


        x_8 = NNANS_8
        y_8 = ieee_logb(x_8)
        if (iy_8 /= iNNANQ_8)error stop 8
                                         !ieee_logb failed real*8 w/ PNANS

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (90+i)
        end do


        call ieee_set_flag(ieee_all,.false.)

        x_16 = PNANQ_16
        y_16 = ieee_logb(x_16)
        if (iy_16(1) /= iPNANQ_16(1))error stop 9
                                         !"ieee_logb failed real*16 w/ PNANQ

        x_16 = NNANQ_16
        y_16 = ieee_logb(x_16)
        if (iy_16(1) /= iNNANQ_16(1))error stop 10
                                         !"ieee_logb failed real*16 w/ NNANQ

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 40
        enddo


        call ieee_set_flag(ieee_all,.false.)

        expect_value = (/.false.,.false.,.true.,.false.,.false./)

        x_16 = PNANS_16
        y_16 = ieee_logb(x_16)
        if (iy_16(1) /= iPNANQ_16(1))error stop 11
                                         !"ieee_logb failed real*16 w/ PNANS

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (100+i)
        end do

        call ieee_set_flag(ieee_all,.false.)

        x_16 = NNANS_16
        y_16 = ieee_logb(x_16)
        if (iy_16(1) /= iNNANQ_16(1))error stop 12
                                         !"ieee_logb failed real*16 w/ NNANS

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) call zzrc (110+i)
        end do



! Test Infinities

        call ieee_set_flag(ieee_all,.false.)


        x_4 = PINF_4
        y_4 = ieee_logb(x_4)
        if (iy_4 /= iPINF_4)error stop 13
                                         !ieee_logb failed real*4 w/PINF_4

        x_4 = NINF_4
        y_4 = ieee_logb(x_4)
        if (iy_4 /= iPINF_4)error stop 14
                                         !ieee_logb failed real*4 w/NINF_4


        x_8 = PINF_8
        y_8 = ieee_logb(x_8)
        if (iy_8 /= iPINF_8)error stop 15
                                         !ieee_logb failed real*8 w/PINF_8


        x_8 = NINF_8
        y_8 = ieee_logb(x_8)
        if (iy_8 /= iPINF_8)error stop 16
                                         !ieee_logb failed real*8 w/NINF_8


        x_16 = z"7ff00000000000000000000000000000"
        y_16 = ieee_logb(x_16)
        if (y_16 /= z"7ff00000000000000000000000000000")error stop 17
                                         !ieee_logb failed real*16 w/PINF_16


        x_16 = z"fff00000000000000000000000000000"
        y_16 = ieee_logb(x_16)
        if (y_16 /= z"7ff00000000000000000000000000000")error stop 18
                                         !ieee_logb failed real*16 w/NINF_16


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 50
        enddo

	end program

