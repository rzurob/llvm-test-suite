! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_FINITE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nans:nofold -qautodbl=dblpad8
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_IS_FINITE for REAL(8)
!*                               which is resulting from conversion
!*                               of REAL(4) using -qautodbl=dblpad4 option.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fxfinite07

        use ieee_arithmetic
        use constants_for_ieee

        real(4), parameter :: pos_r1 = tiny(1.0)
        real(4), parameter :: pos_r2 = huge(1.0)
        real(4), dimension(4) ::  arrval
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


!...test with PZERO and NZERO values
        if (ieee_support_datatype(PZERO_8) .AND. ieee_support_datatype(NZERO_8)) then
           if (ieee_is_finite(PZERO_8) .neqv. .true.) then
              error stop 2
           endif
           if (ieee_is_finite(NZERO_8) .neqv. .true. ) then
              error stop 3
           endif
        endif

!...test with positive reals

        if (ieee_support_datatype(pos_r1)) then
           if (ieee_is_finite(pos_r1) .neqv. .true. ) then
              error stop 4
           endif
        endif

       if (ieee_support_datatype(pos_r2)) then
           if (ieee_is_finite(pos_r2) .neqv. .true.) then
              error stop 5
           endif
       endif

!...test with negative reals

        if (ieee_support_datatype(neg_r1)) then
           if (ieee_is_finite(neg_r1) .neqv. .true. ) then
              error stop 6
           endif
        endif

       if (ieee_support_datatype(neg_r2)) then
           if (ieee_is_finite(neg_r2) .neqv. .true.) then
              error stop 7
           endif
       endif

       if (ieee_support_datatype(neg_r3)) then
           if (ieee_is_finite(neg_r3) .neqv. .true. ) then
              error stop 8
           endif
        endif

       if (ieee_support_datatype(neg_r4)) then
           if (ieee_is_finite(neg_r4) .neqv. .true.) then
              error stop 9
           endif
       endif

!...Check that no flags were turned on by IEEE_IS_FINITE
        call ieee_get_flag(ieee_all, actual_flag_values)
        do k = 1,5
           if (actual_flag_values(k) .neqv. .false. ) then
              error stop 10
           endif
        enddo

        end program
