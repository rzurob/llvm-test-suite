! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NORMAL
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nans:nofold -qrealsize=8
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_IS_NORMAL for REAL(8),
!*                               using -qrealsize=8 compiler option.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        include 'ieeeconsts.h'

        program fnormal05

        use ieee_arithmetic
        use constants_for_ieee

        real, dimension(5) :: normal_result
        real, parameter :: normal_pos1 = tiny(1.0)
        real, parameter :: normal_pos2 = huge(1.0)
        real, dimension(4) :: arrval
        logical :: actual_results(4), actual_flag_values(5)
        integer :: k
        real :: test_pinf, test_ninf
        real :: test_phd, test_ptd, test_nhd, test_ntd
        real :: test_pzero, test_nzero
        real :: test_pnanq_lowest, test_nnaq_lowest
        real :: test_pnanq_highest, test_nnaq_highest
        real :: test_pnans_lowest, test_nnans_lowest
        real :: test_pnans_highest, test_nnans_highest


!... Check  that all flags are false
!... if not, set to .false.

     call ieee_get_flag(ieee_all, actual_flag_values)
     do k = 1, 5
        if (actual_flag_values(k) .neqv. .false. ) then
           call ieee_set_flag(ieee_all(k), .false. )
        endif
     enddo

!...test with PINF and NINF values
        test_pinf = PINF_8
        test_ninf = NINF_8

        if (ieee_support_datatype(test_pinf) .AND. ieee_support_datatype(test_ninf)) then
           if (ieee_is_normal(test_pinf) .eqv. .true. ) then
              error stop 1
           endif

           if (ieee_is_normal(test_ninf) .eqv. .true. ) then
              error stop 2
           endif
        endif

!...test with PHD, PTD, NHD, NTD values
        test_phd = PHD_8
        test_ptd = PTD_8
        test_nhd = NHD_8
        test_ntd = NTD_8

        if (ieee_support_datatype(test_phd) .AND. ieee_support_datatype(test_ptd)) then
           if (ieee_is_normal(test_phd) .eqv. .true. ) then
              error stop 3
           endif
           if (ieee_is_normal(test_ptd) .eqv. .true. ) then
              error stop 4
           endif
       endif


       if (ieee_support_datatype(test_nhd) .AND. ieee_support_datatype(test_ntd)) then
           if (ieee_is_normal(test_nhd) .eqv. .true.) then
              error stop 5
           endif
           if (ieee_is_normal(test_ntd) .eqv. .true.) then
              error stop 6
           endif
       endif

!...test with PZERO_8 and NZERO_8 values
        test_pzero = PZERO_8
        test_nzero = NZERO_8

        if (ieee_support_datatype(test_pzero) .AND. ieee_support_datatype(test_nzero)) then
           if (ieee_is_normal(test_pzero) .neqv. .true.) then
              error stop 7
           endif
           if (ieee_is_normal(test_nzero) .neqv. .true. ) then
              error stop 8
           endif
        endif

!...test with normal values resulted from tiny and huge
        if (ieee_support_datatype(normal_pos1)) then
           if (ieee_is_normal(normal_pos1) .neqv. .true. ) then
              error stop 9
           endif
        endif

       if (ieee_support_datatype(normal_pos2)) then
           if (ieee_is_normal(normal_pos2) .neqv. .true.) then
              error stop 10
           endif
       endif

!...test with arrays
        arrval = (/ PNANQ_8, PNANS_8, NNANQ_8, NNANS_8 /)
        if (ieee_support_datatype(arrval)) then
           actual_results = ieee_is_normal(arrval)
           if (actual_results(1) .neqv. .false.) then
              error stop 11
           endif
           if (actual_results(2) .neqv. .false.) then
              error stop 12
           endif
           if (actual_results(3) .neqv. .false.) then
              error stop 13
           endif
           if (actual_results(4) .neqv. .false.) then
              error stop 14
           endif
        endif

!...test IEEE_IS_NORMAL with normal values resulting from operations
        normal_result(1) = PNORMAL1_8/PNORMAL2_8
        if (ieee_support_datatype(normal_result)) then
           if (ieee_is_normal(normal_result(1)) .eqv. .false. ) then
              error stop 15
           endif
        endif

        normal_result(2) = NNORMAL1_8/PNORMAL2_8
        if (ieee_support_datatype(normal_result)) then
           if (ieee_is_normal(normal_result(2)) .eqv. .false. ) then
              error stop 16
           endif
        endif

        normal_result(3) = NNORMAL1_8/NNORMAL2_8
        if (ieee_support_datatype(normal_result)) then
           if (ieee_is_normal(normal_result(3)) .eqv. .false. ) then
              error stop 17
           endif
        endif

        normal_result(4) = PNORMAL1_8/NNORMAL2_8
        if (ieee_support_datatype(normal_result)) then
           if (ieee_is_normal(normal_result(4)) .eqv. .false. ) then
              error stop 18
           endif
        endif

        normal_result(5) = PNORMAL1_8 - PNORMAL1_8
        if (ieee_support_datatype(normal_result)) then
           if (ieee_is_normal(normal_result(5)) .eqv. .false. ) then
              error stop 19
           endif
        endif

!...test the range values for NANQ
!...lowest range values
        test_pnanq_lowest = pnanq_lowest_8
        if (ieee_support_datatype(test_pnanq_lowest)) then
           if ( ieee_is_normal(test_pnanq_lowest) .eqv. .true. ) then
              error stop 20
           endif
        endif

        test_nnanq_lowest = nnanq_lowest_8
        if (ieee_support_datatype(test_nnanq_lowest)) then
           if ( ieee_is_normal(test_nnanq_lowest) .eqv. .true. ) then
              error stop 21
           endif
        endif

!...highest range values
        test_pnanq_highest = pnanq_highest_8
        if (ieee_support_datatype(test_pnanq_highest)) then
           if ( ieee_is_normal(test_pnanq_highest) .eqv. .true. ) then
              error stop 22
           endif
        endif

        test_nnanq_highest = nnanq_highest_8
        if (ieee_support_datatype(test_nnanq_highest)) then
           if ( ieee_is_normal(nnanq_highest_8) .eqv. .true. ) then
              error stop 23
           endif
        endif

!...test the range values for NANS
!...lowest range values

        test_pnans_lowest = pnans_lowest_8
        if (ieee_support_datatype(test_pnans_lowest)) then
           if ( ieee_is_normal(test_pnans_lowest) .eqv. .true. ) then
              error stop 24
           endif
        endif

        test_nnans_lowest = nnans_lowest_8
        if (ieee_support_datatype(test_nnans_lowest)) then
           if ( ieee_is_normal(test_nnans_lowest) .eqv. .true. ) then
              error stop 25
           endif
        endif

!...highest range values

        test_pnans_highest = pnans_highest_8
        if (ieee_support_datatype(test_pnans_highest)) then
           if ( ieee_is_normal(test_pnans_highest) .eqv. .true. ) then
              error stop 26
           endif
        endif

        test_nnans_highest = nnans_highest_8
        if (ieee_support_datatype(test_nnans_highest)) then
           if ( ieee_is_normal(test_nnans_highest) .eqv. .true. ) then
              error stop 27
           endif
        endif

!...Check that no flags were turned on by IEEE_IS_NORMAL
        call ieee_get_flag(ieee_all, actual_flag_values)
        do k = 1,5
           if (actual_flag_values(k) .neqv. .false. ) then
              error stop 28
           endif
        enddo

        end program
