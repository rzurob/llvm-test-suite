! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxi3e2.presh fxfinite02
! %COMPOPTS: -qfloat=nans:nofold -qfree=f90 -qstrict
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_FINITE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nans:nofold
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_IS_FINITE with DOUBLE
!*                               PRECISION real.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fxfinite06

        use ieee_arithmetic
        use constants_for_ieee

        double precision plus_nanq, minus_nanq, plus_nans, minus_nans
        double precision, parameter :: pos_r1 = tiny(1.0)
        double precision, parameter :: pos_r2 = huge(1.0)
        double precision, dimension(4) :: arrval
        double precision :: neg_r1 = -1.0, neg_r2 = -0.1
        double precision :: neg_r3 = -tiny(1.0), neg_r4 = -huge(1.0)
        logical :: actual_results(4), actual_flag_values(5)
        integer :: k

!... Check  that all flags are false

     call ieee_get_flag(ieee_all, actual_flag_values)
     do k = 1, 5
        if (actual_flag_values(k) .neqv. .false. ) then
           error stop 1
        endif
     enddo

!...test with PINF_8 and NINF_8 values
        if (ieee_support_datatype(PINF_8) .AND. ieee_support_datatype(NINF_8)) then
           if (ieee_is_finite(PINF_8) .OR. ieee_is_finite(NINF_8)) then
              error stop 2
           endif
        endif

!...test with PHD_8, PTD_8, NHD_8, NTD_8 values
       if (ieee_support_datatype(PHD_8) .AND. ieee_support_datatype(PTD_8)) then
           if (ieee_is_finite(PHD_8) .neqv. .true.) then
              error stop 3
           endif
           if (ieee_is_finite(PTD_8) .neqv. .true.) then
              error stop 4
           endif
       endif

       if (ieee_support_datatype(NHD_8) .AND. ieee_support_datatype(NTD_8)) then
           if (ieee_is_finite(NHD_8) .neqv. .true.) then
              error stop 5
           endif
           if (ieee_is_finite(NTD_8) .neqv. .true.) then
              error stop 6
           endif
       endif

!...test with PZERO_8 and NZERO_8 values
        if (ieee_support_datatype(PZERO_8) .AND. ieee_support_datatype(NZERO_8)) then
           if (ieee_is_finite(PZERO_8) .neqv. .true.) then
              error stop 7
           endif
           if (ieee_is_finite(NZERO_8) .neqv. .true. ) then
              error stop 8
           endif
        endif

!...test with positive normal
        if (ieee_support_datatype(pos_r1)) then
           if (ieee_is_finite(pos_r1) .neqv. .true. ) then
              error stop 9
           endif
        endif

       if (ieee_support_datatype(pos_r2)) then
           if (ieee_is_finite(pos_r2) .neqv. .true.) then
              error stop 10
           endif
       endif

!...test with negative normal
       if (ieee_support_datatype(neg_r1)) then
           if (ieee_is_finite(neg_r1) .neqv. .true. ) then
              error stop 11
           endif
       endif

       if (ieee_support_datatype(neg_r2)) then
           if (ieee_is_finite(neg_r2) .neqv. .true.) then
              error stop 12
           endif
       endif

       if (ieee_support_datatype(neg_r3)) then
           if (ieee_is_finite(neg_r3) .neqv. .true. ) then
              error stop 13
           endif
        endif

       if (ieee_support_datatype(neg_r4)) then
           if (ieee_is_finite(neg_r4) .neqv. .true.) then
              error stop 14
           endif
       endif

!...test with arrays
        arrval = (/ PNANQ_8, PNANS_8, NNANQ_8, NNANS_8 /)
        if (ieee_support_datatype(arrval)) then
           actual_results = ieee_is_finite(arrval)
           if (actual_results(1) .neqv. .false.) then
              error stop 15
           endif
           if (actual_results(2) .neqv. .false.) then
              error stop 16
           endif
           if (actual_results(3) .neqv. .false.) then
              error stop 17
           endif
           if (actual_results(4) .neqv. .false.) then
              error stop 18
           endif
        endif

!...test the range values for NANQ
!...lowest range values
        plus_nanq = z'7FF8000000000000'
        if ( ieee_is_finite(plus_nanq) .eqv. .true. ) then
           error stop 19
        endif

        minus_nanq = z'FFF8000000000000'
        if ( ieee_is_finite(minus_nanq) .eqv. .true. ) then
           error stop 20
        endif

!...highest range values
        plus_nanq = z'7FFFFFFFFFFFFFFF'
        if ( ieee_is_finite(plus_nanq) .eqv. .true. ) then
           error stop 21
        endif

        minus_nanq = z'FFFFFFFFFFFFFFFF'
        if ( ieee_is_finite(minus_nanq) .eqv. .true. ) then
           error stop 22
        endif

!...test the range values for NANS
!...lowest range values
        plus_nans = z'7FF0000000000001'

        if ( ieee_is_finite(plus_nans) .eqv. .true. ) then
           error stop 23
        endif

        minus_nans = z'FFF0000000000001'
        if ( ieee_is_finite(minus_nans) .eqv. .true. ) then
           error stop 24
        endif

!...highest range values
        plus_nans = z'7FF7FFFFFFFFFFFF'
        if ( ieee_is_finite(plus_nans) .eqv. .true. ) then
           error stop 25
        endif

        minus_nans = z'FFF7FFFFFFFFFFFF'
        if ( ieee_is_finite(minus_nans) .eqv. .true. ) then
           error stop 26
        endif

!...Check that no flags were turned on by IEEE_IS_FINITE
        call ieee_get_flag(ieee_all, actual_flag_values)
        do k = 1,5
           if (actual_flag_values(k) .neqv. .false. ) then
              error stop 27
           endif
        enddo

        end program
