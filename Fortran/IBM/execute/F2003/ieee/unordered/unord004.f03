! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 8, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_UNORDERED with real*4 and no NaNs.
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
        implicit none
        real*4, parameter, dimension(10) :: value1 = &
     &       (/             &
     &       PINF_4,        & ! Positive INF
     &       NINF_4,         & ! Negative INF
     &       huge(PINF_4),  & ! Positive Normal
     &       tiny(PINF_4),  & ! Positive Normal
     &       PHD_4,         & ! Positive Denormal
     &       NHD_4,         & ! Negative Denormal
     &       PTD_4,         & ! Positive Denormal
     &       NTD_4,         & ! Negative Denormal
     &       PZERO_4,       & ! Positive Zero
     &       NZERO_4       & ! Negative Zero
     &       /)

	real*4 :: value2
        logical :: result, flag_values(5)

        integer :: i

        ! ieee_unordered should not set any flags.  Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)

!  Test real*4
!**********************
        call ieee_set_flag(ieee_all,.false.)

        value2 = PINF_4
        result = ieee_unordered(value1(2), value2)
            if ( result .eqv. .true.) error stop 1
               !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(20+i)
              enddo


!**********************

!**********************
        call ieee_set_flag(ieee_all,.false.)

        value2 = NINF_4
        result = ieee_unordered(value1(2), value2)
            if ( result .eqv. .true.) error stop 2
               !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(30+i)
              enddo


!**********************

!**********************
        call ieee_set_flag(ieee_all,.false.)

        value2 = huge(PINF_4)
        result = ieee_unordered(value1(2), value2)
            if ( result .eqv. .true.) error stop 3
               !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(40+i)
              enddo


!**********************

!**********************
        call ieee_set_flag(ieee_all,.false.)

        value2 = tiny(PINF_4)
        result = ieee_unordered(value1(1), value2)
            if ( result .eqv. .true.) error stop 4
               !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(50+i)
              enddo


!**********************

!**********************
        call ieee_set_flag(ieee_all,.false.)

        value2 = PHD_4
        result = ieee_unordered(value1(3), value2)
            if ( result .eqv. .true.) error stop 5
               !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(60+i)
              enddo


!**********************

!**********************
        call ieee_set_flag(ieee_all,.false.)

        value2 = NHD_4
        result = ieee_unordered(value1(4), value2)
            if ( result .eqv. .true.) error stop 6
               !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(60+i)
              enddo


!**********************

!**********************
        call ieee_set_flag(ieee_all,.false.)

        value2 = PTD_4
        result = ieee_unordered(value1(5), value2)
            if ( result .eqv. .true.) error stop 7
               !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(70+i)
              enddo


!**********************

!**********************
        call ieee_set_flag(ieee_all,.false.)

        value2 = NTD_4
        result = ieee_unordered(value1(6), value2)
            if ( result .eqv. .true.) error stop 8
               !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(80+i)
              enddo


!**********************

        call ieee_set_flag(ieee_all,.false.)

        value2 = NZERO_4
        result = ieee_unordered(value1(7), value2)
            if ( result .eqv. .true.) error stop 9
               !ieee_unordered error

        call ieee_get_flag(ieee_all,flag_values)
              do i = 1,5
                  if (flag_values(i) .neqv. .false.)call zzrc(90+i)
              enddo


	end