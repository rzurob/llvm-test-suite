! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxi3e2.presh fxfinite01
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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Vasile Radulescu
!*  DATE                       : February 15, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_FINITE 
!*  SECONDARY FUNCTIONS TESTED : 
!*                               
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nans:nofold
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_IS_FINITE for REAL(4).
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fxfinite01
      
        use ieee_arithmetic
        use constants_for_ieee
        
        real(4) plus_nanq, minus_nanq, plus_nans, minus_nans       
        real(4), parameter :: pos_r1 = tiny(1.0)
        real(4), parameter :: pos_r2 = huge(1.0)
        real(4), dimension(4) :: arrval
        real(4) :: neg_r1 = -1.0, neg_r2 = -0.1
        real(4) :: neg_r3 = -tiny(1.0), neg_r4 = -huge(1.0)
        logical :: actual_results(4), actual_flag_values(5)
        integer :: k

!... Check  that all flags are false

     call ieee_get_flag(ieee_all, actual_flag_values)
     do k = 1, 5
        if (actual_flag_values(k) .neqv. .false. ) then
           error stop 1
        endif
     enddo

!...test with PINF and NINF values
        if (ieee_support_datatype(PINF_4) .AND. ieee_support_datatype(NINF_4)) then
           if (ieee_is_finite(PINF_4) .OR. ieee_is_finite(NINF_4)) then
              error stop 2 
           endif
        endif

!...test with PHD, PTD, NHD, NTD values        
       if (ieee_support_datatype(PHD_4) .AND. ieee_support_datatype(PTD_4)) then
           if (ieee_is_finite(PHD_4) .neqv. .true.) then
              error stop 3
           endif
           if (ieee_is_finite(PTD_4) .neqv. .true.) then
              error stop 4
           endif
       endif
        
       if (ieee_support_datatype(NHD_4) .AND. ieee_support_datatype(NTD_4)) then
           if (ieee_is_finite(NHD_4) .neqv. .true.) then
              error stop 5
           endif
           if (ieee_is_finite(NTD_4) .neqv. .true.) then
              error stop 6
           endif
       endif

!...test with PZERO and NZERO values       
        if (ieee_support_datatype(PZERO_4) .AND. ieee_support_datatype(NZERO_4)) then
           if (ieee_is_finite(PZERO_4) .neqv. .true.) then
              error stop 7
           endif
           if (ieee_is_finite(NZERO_4) .neqv. .true. ) then
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
        arrval = (/ PNANQ_4, PNANS_4, NNANQ_4, NNANS_4 /)
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
        plus_nanq = z'7FC00000'
        if ( ieee_is_finite(plus_nanq) .eqv. .true. ) then
           error stop 19
        endif

        minus_nanq = z'FFC00000'
        if ( ieee_is_finite(minus_nanq) .eqv. .true. ) then
           error stop 20
        endif

!...highest range values
        plus_nanq = z'7FFFFFFF'
        if ( ieee_is_finite(plus_nanq) .eqv. .true. ) then
           error stop 21
        endif

        minus_nanq = z'FFFFFFFF'
        if ( ieee_is_finite(minus_nanq) .eqv. .true. ) then
           error stop 22
        endif

!...test the range values for NANS
!...lowest range values
        plus_nans = z'7F800001'

        if ( ieee_is_finite(plus_nans) .eqv. .true. ) then
           error stop 23
        endif

        minus_nans = z'FF800001'
        if ( ieee_is_finite(minus_nans) .eqv. .true. ) then
           error stop 24
        endif

!...highest range values
        plus_nans = z'7FBFFFFF'
        if ( ieee_is_finite(plus_nans) .eqv. .true. ) then
           error stop 25
        endif

        minus_nans = z'FFBFFFFF'
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
