! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 8, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_UNORDERED
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
!234567890123456789012345678901234567890123456789012345678901234567890
	    program fxieee21

        use ieee_arithmetic
        use constants_for_ieee

        real*4, parameter, dimension(16) :: values1 = &
     &       (/             &
     &       PNANS_4,       & ! Signaling NaN
     &       NNANS_4,       & ! Signaling NaN
     &       PNANQ_4,       & ! Quiet NaN
     &       NNANQ_4,       & ! Quiet NaN
     &       PINF_4,        & ! Positive INF
     &       huge(PINF_4),  & ! Positive Normal
     &       tiny(PINF_4),  & ! Positive Normal
     &       PHD_4,         & ! Positive Denormal
     &       PNANS_4,       & ! Signaling NaN
     &       NNANS_4,       & ! Signaling NaN
     &       PNANQ_4,       & ! Quiet NaN
     &       NNANQ_4,       & ! Quiet NaN
     &       NHD_4,         & ! Negative Denormal
     &       -tiny(PINF_4), & ! Negative Normal
     &       -huge(PINF_4), & ! Negative Normal
     &       NINF_4         & ! Negative INF
     &       /)

        real*4, parameter, dimension(16) :: values2 = &
     &       (/             &
     &       PINF_4,        & ! Positive INF
     &       huge(PINF_4),  & ! Positive Normal
     &       tiny(PINF_4),  & ! Positive Normal
     &       PHD_4,         & ! Positive Denormal
     &       PNANS_4,       & ! Signaling NaN
     &       NNANS_4,       & ! Signaling NaN
     &       PNANQ_4,       & ! Quiet NaN
     &       NNANQ_4,       & ! Quiet NaN
     &       PNANS_4,       & ! Signaling NaN
     &       NNANS_4,       & ! Signaling NaN
     &       PNANQ_4,       & ! Quiet NaN
     &       NNANQ_4,       & ! Quiet NaN
     &       PTD_4,         & ! Positive Denormal
     &       PZERO_4,       & ! Positive Zero
     &       NZERO_4,       & ! Negative Zero
     &       NTD_4          & ! Negative Denormal
     &       /)

        logical(4), dimension(16) :: results, flag_values(5)

        real*8, parameter, dimension(16) :: values1_8 = &
     &       (/             &
     &       PNANS_8,       & ! Signaling NaN
     &       NNANS_8,       & ! Signaling NaN
     &       PNANQ_8,       & ! Quiet NaN
     &       NNANQ_8,       & ! Quiet NaN
     &       PINF_8,        & ! Positive INF
     &       huge(PINF_8),  & ! Positive Normal
     &       tiny(PINF_8),  & ! Positive Normal
     &       PHD_8,         & ! Positive Denormal
     &       PNANS_8,       & ! Signaling NaN
     &       NNANS_8,       & ! Signaling NaN
     &       PNANQ_8,       & ! Quiet NaN
     &       NNANQ_8,       & ! Quiet NaN
     &       NHD_8,         & ! Negative Denormal
     &       -tiny(PINF_8), & ! Negative Normal
     &       -huge(PINF_8), & ! Negative Normal
     &       NINF_8         & ! Negative INF
     &       /)

        real*8, parameter, dimension(16) :: values2_8 = &
     &       (/             &
     &       PINF_8,        & ! Positive INF
     &       huge(PINF_8),  & ! Positive Normal
     &       tiny(PINF_8),  & ! Positive Normal
     &       PHD_8,         & ! Positive Denormal
     &       PNANS_8,       & ! Signaling NaN
     &       NNANS_8,       & ! Signaling NaN
     &       PNANQ_8,       & ! Quiet NaN
     &       NNANQ_8,       & ! Quiet NaN
     &       PNANS_8,       & ! Signaling NaN
     &       NNANS_8,       & ! Signaling NaN
     &       PNANQ_8,       & ! Quiet NaN
     &       NNANQ_8,       & ! Quiet NaN
     &       PTD_8,         & ! Positive Denormal
     &       PZERO_8,       & ! Positive Zero
     &       NZERO_8,       & ! Negative Zero
     &       NTD_8          & ! Negative Denormal
     &       /)

        real*16, parameter, dimension(16) :: values1_16 = &
     &       (/             &
     &       PNANS_16,       & ! Signaling NaN
     &       NNANS_16,       & ! Signaling NaN
     &       PNANQ_16,       & ! Quiet NaN
     &       NNANQ_16,       & ! Quiet NaN
     &       PINF_16,        & ! Positive INF
     &       huge(PINF_16),  & ! Positive Normal
     &       tiny(PINF_16),  & ! Positive Normal
     &       PZERO_16,       & ! Positive Zero
     &       PNANS_16,       & ! Signaling NaN
     &       NNANS_16,      & ! Signaling NaN
     &       PNANQ_16,      & ! Quiet NaN
     &       NNANQ_16,       & ! Quiet NaN
     &       PINF_16,        & ! Positive INF
     &       huge(PINF_16),  & ! Positive Normal
     &       tiny(PINF_16),  & ! Positive Normal
     &       PZERO_16        & ! Positive Zero
     &       /)

        real*16, parameter, dimension(16) :: values2_16 = &
     &       (/             &
     &       PZERO2_16,      & ! Positive Zero
     &       -tiny(PINF_16), & ! Negative Normal
     &       -huge(PINF_16), & ! Negative Normal
     &       NINF_16,        & ! Negative INF
     &       PNANS_16,       & ! Signaling NaN
     &       NNANS_16,       & ! Signaling NaN
     &       PNANQ_16,       & ! Quiet NaN
     &       NNANQ_16,       & ! Quiet NaN
     &       PNANS_16,       & ! Signaling NaN
     &       NNANS_16,      & ! Signaling NaN
     &       PNANQ_16,      & ! Quiet NaN
     &       NNANQ_16,       & ! Quiet NaN
     &       PZERO2_16,      & ! Positive Zero
     &       -tiny(PINF_16), & ! Negative Normal
     &       -huge(PINF_16), & ! Negative Normal
     &       NINF_16         & ! Negative INF
     &       /)

        integer :: i

        ! ieee_unordered should not set any flags.  Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)

!  Test real*4

        results = ieee_unordered(values1, values2)
        do i = 1, 12
            if ( results(i) .eqv. .false.) call zzrc(i)
        end do

        do i = 13, 16
            if ( results(i) .eqv. .true.) call zzrc(i)
        end do

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(50+i)
              enddo


!       Test real*8

        call ieee_set_flag(ieee_all,.false.)

        results = ieee_unordered(values1_8, values2_8)
        do i = 1, 12
            if ( results(i) .eqv. .false.) call zzrc(i+10)
        end do

        do i = 13, 16
            if ( results(i) .eqv. .true.) call zzrc(i+10)
        end do

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(60+i)
              enddo

!       Test array of kind 16

        call ieee_set_flag(ieee_all,.false.)

        results = ieee_unordered(values1_16, values2_16)
         do i = 1, 12
            if ( results(i) .eqv. .false.) call zzrc(i+20)
         end do

	 do i = 13, 16
            if ( results(i) .eqv. .true.) call zzrc(i+20)
         end do


        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(70+i)
              enddo

        end program
