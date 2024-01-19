! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 5, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_LOGB
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
       program fxieee10

        use ieee_arithmetic
        use constants_for_ieee

        real*4 :: xr_4, yr_4
        real*8 :: xr_8, yr_8
        real*16 :: xr_16, yr_16
        real*4, parameter, dimension(12) :: values = &
     &       (/             &
     &       PNANS_4,       & ! Signaling NaN
     &       NNANS_4,       & ! Signaling NaN
     &       PNANQ_4,       & ! Quiet NaN
     &       NNANQ_4,       & ! Quiet NaN
     &       PINF_4,        & ! Positive INF
     &       PHD_4,         & ! Positive Denormal
     &       PTD_4,         & ! Positive Denormal
     &       PZERO_4,       & ! Positive Zero
     &       NZERO_4,       & ! Negative Zero
     &       NTD_4,         & ! Negative Denormal
     &       NHD_4,         & ! Negative Denormal
     &       NINF_4         & ! Negative INF
     &       /)

        real(4), dimension(12) :: xresults
        integer*4, dimension(12) :: iresults

      real*8, parameter, dimension(12) :: values_8 = &
     &       (/             &
     &       PNANS_8,       & ! Signaling NaN
     &       NNANS_8,       & ! Signaling NaN
     &       PNANQ_8,       & ! Quiet NaN
     &       NNANQ_8,       & ! Quiet NaN
     &       PINF_8,        & ! Positive INF
     &       PHD_8,         & ! Positive Denormal
     &       PTD_8,         & ! Positive Denormal
     &       PZERO_8,       & ! Positive Zero
     &       NZERO_8,       & ! Negative Zero
     &       NTD_8,         & ! Negative Denormal
     &       NHD_8,         & ! Negative Denormal
     &       NINF_8         & ! Negative INF
     &       /)

        real*8, dimension(12) :: xresults_8
        integer*8, dimension(12) :: iresults_8

        real*16, parameter, dimension(8) :: values_16 = &
     &       (/             &
     &       PNANS_16,       & ! Signaling NaN
     &       NNANS_16,       & ! Signaling NaN
     &       PNANQ_16,       & ! Quiet NaN
     &       NNANQ_16,       & ! Quiet NaN
     &       PINF_16,        & ! Positive INF
     &       PZERO_16,       & ! Positive Zero
     &       PZERO2_16,      & ! Positive Zero
     &       NINF_16         & ! Negative INF
     &       /)

        real*16, dimension(8) :: xresults_16
        integer*8 :: apns(2), anns(2), apnq(2), annq(2)

        logical, dimension(5) :: flag_values
        integer :: i

        equivalence (iresults, xresults)
        equivalence (iresults_8, xresults_8)
    equivalence (apns, xresults_16(1))
    equivalence (anns, xresults_16(2))
    equivalence (apnq, xresults_16(3))
        equivalence (annq, xresults_16(4))

!       Test real*4
        xr_4 = 3.0
        yr_4 = ieee_logb(xr_4)
        if (yr_4 /= exponent(xr_4)-1) print *, "ieee_logb failed for real*4."

    xr_4 = huge(1.0)
        yr_4 = ieee_logb(xr_4)
        if (yr_4 /= exponent(xr_4)-1) print *, "ieee_logb failed for real*4."

        xr_4 = tiny(1.0)
        yr_4 = ieee_logb(xr_4)
        if (yr_4 /= exponent(xr_4)-1) print *, "ieee_logb failed for real*4."

!       Test real*8
        xr_8 = 65536.0
        yr_8 = ieee_logb(xr_8)
        if (yr_8 /= exponent(xr_8)-1) print *, "ieee_logb failed for real*8."

        xr_8 = huge(1.0_8)
        yr_8 = ieee_logb(xr_8)
        if (yr_8 /= exponent(xr_8)-1) print *, "ieee_logb failed for real*8."

        xr_8 = tiny(1.0_8)
        yr_8 = ieee_logb(xr_8)
        if (yr_8 /= exponent(xr_8)-1) print *, "ieee_logb failed for real*8."

!       test real*16
        xr_16 = 2q0**308
        yr_16 = ieee_logb(xr_16)
        if (yr_16 /= exponent(xr_16)-1) print *, "ieee_logb failed real*16."

        xr_16 = huge(1.0_16)
        yr_16 = ieee_logb(xr_16)
        if (yr_16 /= exponent(xr_16)-1) print *, "ieee_logb failed for real*16."

        xr_16 = tiny(1.0_16)
        yr_16 = ieee_logb(xr_16)
        if (yr_16 /= exponent(xr_16)-1) print *, "ieee_logb failed for real*16."

!       Test real*4
        xr_4 = -3.0
        yr_4 = ieee_logb(xr_4)
        if (yr_4 /= exponent(xr_4)-1) print *, "ieee_logb failed real*4 w/ negative."

!       Test real*8
        xr_8 = -65536d0
        yr_8 = ieee_logb(xr_8)
        if (yr_8 /= exponent(xr_8)-1) print *, "ieee_logb failed real*8 w/ negative."

!       test real*16
        xr_16 = -2q0**308
        yr_16 = ieee_logb(xr_16)
        if (yr_16 /= exponent(xr_16)-1) print *, "ieee_logb failed real*16 w/ negative.."

!       Test array of kind 4
        xresults = ieee_logb(values)

        if (iresults(1) /= iPNANQ_4) print *, "ieee_logb failed at kind 4 array 1."
        if (iresults(2) /= iNNANQ_4) print *, "ieee_logb failed at kind 4 array 2."
        if (iresults(3) /= iPNANQ_4) print *, "ieee_logb failed at kind 4 array 3."
      if (iresults(4) /= iNNANQ_4) print *, "ieee_logb failed at kind 4 array 4."
      if (iresults(5) /= iPINF_4) print *, "ieee_logb failed at kind 4 array 5."
      ! denormal
    if (xresults(6) /= exponent(values(6))-1 ) then
       print *, "ieee_logb failed at kind 4 array 6."
       print '(z8.8)', xresults(6)
    end if

    if (xresults(7) /= exponent(values(7))-1 ) print *, "ieee_logb failed at kind 4 array 7."

    if (iresults(8) /= z"ff800000") print *, "ieee_logb at kind 4 array failed 8."
      if (iresults(9) /= z"ff800000") print *, "ieee_logb at kind 4 array failed 9."

      !  denormal
      if ( xresults(10) /= exponent(values(10))-1 ) print *, "ieee_logb failed 10."
      if ( xresults(11) /= exponent(values(11))-1 ) print *, "ieee_logb failed 11."

      if (iresults(12) /= z"7f800000") print *, "ieee_logb failed at kind 4 array 12."

!       Test array of kind 8
        xresults_8 = ieee_logb(values_8)

     if (iresults_8(1) /= iPNANQ_8) print *, "ieee_logb failed at kind 8 array 1 ."
       if (iresults_8(2) /= iNNANQ_8) print *, "ieee_logb failed at kind 8 array 2."
       if (iresults_8(3) /= iPNANQ_8) print *, "ieee_logb failed at kind 8 array 3."
       if (iresults_8(4) /= iNNANQ_8) print *, "ieee_logb failed at kind 8 array 4."
       if (iresults_8(5) /= iPINF_8) print *, "ieee_logb failed at kind 8 array 5."
       if (xresults_8(6) /= exponent(values_8(6))-1) print *, "ieee_logb failed at kind 8 array 6."
       if (xresults_8(7) /= exponent(values_8(7))-1) print *, "ieee_logb failed at kind 8 array 7."

       !  what about denormal
       if (iresults_8(8) /= z"fff0000000000000" ) print *, "ieee_logb failed at kind 8 array 8."
       if (iresults_8(9) /= z"fff0000000000000" ) print *, "ieee_logb failed at kind 8 array 9."

       if (xresults_8(10) /= exponent(values_8(10))-1) print *, "ieee_logb failed at kind 8 array 10."
       if (xresults_8(11) /= exponent(values_8(11))-1) print *, "ieee_logb failed at kind 8 array 11."

       if (iresults_8(12) /= z"7ff0000000000000" ) print *, "ieee_logb failed at kind 8 array 12."


!       Test array of kind 16
        xresults_16 = ieee_logb(values_16)

        if (apns(1) /= iPNANQ_16(1)) then
           print *, "ieee_logb failed at kind 16 array 1."
           print '(z32.32)', apns(1)
        endif
        if (anns(1) /= iNNANQ_16(1)) then
           print *, "ieee_logb failed at kind 16 array 2."
           print '(z32.32)', anns(1)
       endif
       if (apnq(1) /= iPNANQ_16(1)) then
          print *, "ieee_logb failed at kind 16 array 3."
          print '(z32.32)', apnq(1)
       endif
       if (annq(1) /= iNNANQ_16(1)) then
          print *, "ieee_logb failed at kind 16 array 4."
          print '(z32.32)', annq(1)
       endif
       if (xresults_16(5) /= z"7ff00000000000000000000000000000") then
          print *, "ieee_logb failed at kind 16 array 5."
       endif
       if (xresults_16(6) /= z"FFF0000000000000FFF0000000000000") then
          print *, "ieee_logb failed at kind 16 array 6."
       endif
       if (xresults_16(7) /= z"FFF0000000000000FFF0000000000000") then
          print *, "ieee_logb failed at kind 16 array 7."
       endif
       if (xresults_16(8) /= z"7ff00000000000000000000000000000" ) then
          print *, "ieee_logb failed at kind 16 array 8."
       end if

       end program

