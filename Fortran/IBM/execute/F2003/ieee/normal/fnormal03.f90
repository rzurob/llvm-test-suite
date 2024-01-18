! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NORMAL
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nans:nofold
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_IS_NORMAL for REAL(16).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

     program fnormal03

     use ieee_arithmetic
     use constants_for_ieee

     real(16), dimension(5) :: normal_result
     real(16), parameter :: normal_pos1 = tiny(1.0_16)
     real(16), parameter :: normal_pos2 = huge(1.0_16)
     real(16) :: tiny_denormal_pos, tiny_denormal_neg
     real(16), dimension(4) :: arrval
     logical :: actual_results(4), actual_flag_values(5)
     integer :: k


!... Check  that all flags are false
!... if not, set to .false.

     call ieee_get_flag(ieee_all, actual_flag_values)
     do k = 1, 5
        if (actual_flag_values(k) .neqv. .false. ) then
           call ieee_set_flag(ieee_all(k), .false. )
        endif
     enddo

!...test with PINF and NINF values

     if (ieee_is_normal(PINF_16) .neqv. .false.) then
        error stop 1
     endif
     if (ieee_is_normal(NINF_16) .neqv. .false.) then
        error stop 2
     endif

!...test with PZERO_16 and PZERO2_16 values

     if (ieee_is_normal(PZERO_16) .neqv. .true.) then
        error stop 3
     endif

     if (ieee_is_normal(PZERO2_16) .neqv. .true. ) then
        error stop 4
     endif

!...test with tiniest denormal values (in magnitude)

     tiny_denormal_pos = tiny(1.0_16)/2.0_16
     if (ieee_is_normal(tiny_denormal_pos) .neqv. .false. ) then
        error stop 5
     endif

     tiny_denormal_neg = -tiny(1.0_16)/2.0_16
     if (ieee_is_normal(tiny_denormal_neg) .neqv. .false. ) then
        error stop 6
     endif

!...test with normal values

     if (ieee_is_normal(normal_pos1) .neqv. .true. ) then
        error stop 7
     endif


     if (ieee_is_normal(normal_pos2) .neqv. .true.) then
        error stop 8
     endif

!...test with arrays
     arrval = (/ PNANQ_16, PNANS_16, NNANQ_16, NNANS_16 /)

     actual_results = ieee_is_normal(arrval)
     if (actual_results(1) .neqv. .false.) then
        error stop 9
     endif
     if (actual_results(2) .neqv. .false.) then
        error stop 10
     endif
     if (actual_results(3) .neqv. .false.) then
        error stop 11
     endif
     if (actual_results(4) .neqv. .false.) then
        error stop 12
     endif

!...test IEEE_IS_NORMAL with normal values resulting from operations
     normal_result(1) = PNORMAL1_16/PNORMAL2_16

     if (ieee_is_normal(normal_result(1)) .eqv. .false. ) then
        error stop 13
     endif

     normal_result(2) = NNORMAL1_16/PNORMAL2_16

     if (ieee_is_normal(normal_result(2)) .eqv. .false. ) then
        error stop 14
     endif

     normal_result(3) = NNORMAL1_16/NNORMAL2_16

     if (ieee_is_normal(normal_result(3)) .eqv. .false. ) then
        error stop 15
     endif


     normal_result(4) = PNORMAL1_16/NNORMAL2_16

     if (ieee_is_normal(normal_result(4)) .eqv. .false. ) then
        error stop 16
     endif

     normal_result(5) = PNORMAL1_16 - PNORMAL1_16

     if (ieee_is_normal(normal_result(5)) .eqv. .false. ) then
        error stop 17
     endif

!...test the range values for NANQ
!...lowest range values

     if ( ieee_is_normal(pnanq_lowest_16) .eqv. .true. ) then
        error stop 18
     endif

     if ( ieee_is_normal(nnanq_lowest_16) .eqv. .true. ) then
        error stop 19
     endif

!...highest range values

     if ( ieee_is_normal(pnanq_highest_16) .eqv. .true. ) then
        error stop 20
     endif

     if ( ieee_is_normal(nnanq_highest_16) .eqv. .true. ) then
        error stop 21
     endif

!...test the range values for NANS
!...lowest range values

     if ( ieee_is_normal(pnans_lowest_16) .eqv. .true. ) then
        error stop 22
     endif

     if ( ieee_is_normal(nnans_lowest_16) .eqv. .true. ) then
        error stop 23
     endif

!...highest range values

     if ( ieee_is_normal(pnans_highest_16) .eqv. .true. ) then
        error stop 24
     endif

     if ( ieee_is_normal(nnans_highest_16) .eqv. .true. ) then
        error stop 25
     endif

!...Check that no flags were turned on by IEEE_IS_NORMAL
     call ieee_get_flag(ieee_all, actual_flag_values)
     do k = 1,5
        if (actual_flag_values(k) .neqv. .false. ) then
           error stop 26
        endif
     enddo

     end program
