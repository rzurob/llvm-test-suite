! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 7, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SELECTED_REAL_KIND(P,R)
!*                               and defined ranges for P and R.
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : This testcase tests scenarios related to:
!*
!* 1. Values for P & R of real*4
!* 2. Values for P & R of real*8
!* 3. Values for P & R of real*16
!* 4. Values for P & R outside of the available range
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program select_para

        use ieee_arithmetic
        use constants_for_ieee

	logical :: flag_values(5)
        integer ::  i, zi, P, R


        ! ieee_selected_real_kind should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.

        call ieee_set_flag(ieee_all,.false.)

	do P = 0, 6
	   do R = 0, 37
	      zi = ieee_selected_real_kind(P, R)
              if (zi /= 4 ) call zzrc(R)
	                         !ieee_selected_real_kind 4 failed
	   enddo
	 enddo

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 400
        enddo

        call ieee_set_flag(ieee_all,.false.)

	 do P = 0, 6
	    do R = 38, 307
	      zi = ieee_selected_real_kind(P, R)
              if (zi /= 8 ) call zzrc(R)
	                         !ieee_selected_real_kind 8 failed
	   enddo
	 enddo

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 500
        enddo

        call ieee_set_flag(ieee_all,.false.)

         do P = 7, 15
            do R = 0, 307
	        zi = ieee_selected_real_kind(P, R)
                if (zi /= 8 ) call zzrc(R)
	                         !ieee_selected_real_kind 8 failed
	     enddo
	  enddo

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 600
        enddo

        call ieee_set_flag(ieee_all,.false.)

          do P = 16, 31
	     do R = 0, 291
		  zi = ieee_selected_real_kind(P, R)
                  if (zi /= 16 ) call zzrc(R)
	                         !ieee_selected_real_kind 16 failed
	     enddo
	  enddo

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 700
        enddo

        call ieee_set_flag(ieee_all,.false.)

	  P = 32
	  R = 37
          zi = ieee_selected_real_kind(P, R)
	  if (zi /= -1 ) call zzrc(R)
	                         !ieee_selected_real_kind

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 800
        enddo

        call ieee_set_flag(ieee_all,.false.)

          P = 0
	  R = 308
 	  zi = ieee_selected_real_kind(P, R)
          if (zi /= -2 ) call zzrc(R)
	                         !ieee_selected_real_kind

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 900
        enddo

        call ieee_set_flag(ieee_all,.false.)

	  P = 32
	  R = 308
	  zi = ieee_selected_real_kind(P, R)
	  if (zi /= -3 ) call zzrc(R)
	                         !ieee_selected_real_kind

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.)error stop 1000
        enddo

          end program

