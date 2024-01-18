! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh logb003
! %COMPOPTS: -qfree=f90 -qstrict
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
!*  PROGRAMMER                 : Alexandru Mihaileanu
!*  DATE                       : February 5, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_LOGB with arrays of real constants.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* 1. Testing real*4
!* 2. Testing real*8
!* 3. Testing real*16
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program logb_variab

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

        real(4), dimension(12) :: xres
        integer*4, dimension(12) :: ires

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

        real*8, dimension(12) :: xres_8
        integer*8, dimension(12) :: ires_8

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

        real*16, dimension(8) :: xres_16
        integer*8 :: apns(2), anns(2), apnq(2), annq(2)
		
        logical, dimension(5) :: flag_values, expect_value(5)
        integer :: i

        equivalence (ires, xres)
        equivalence (ires_8, xres_8)
	equivalence (apns, xres_16(1))
	equivalence (anns, xres_16(2))
	equivalence (apnq, xres_16(3))
        equivalence (annq, xres_16(4))

!Test array of kind 4

        call ieee_set_flag(ieee_all,.false.)

        expect_value = (/.false.,.true.,.true.,.false.,.false./)

        xres = ieee_logb(values)
		
        if (ires(1) /= iPNANQ_4) error stop 1 
                                     ! "ieee_logb failed for PNANS_4

        if (ires(2) /= iNNANQ_4) error stop 2 
                                     ! "ieee_logb failed for NNANS_4

        if (ires(3) /= iPNANQ_4) error stop 3 
                                     ! "ieee_logb failed for PNANQ_4

	if (ires(4) /= iNNANQ_4) error stop 4 
                                     ! "ieee_logb failed for NNANQ_4

	if (ires(5) /= iPINF_4) error stop 5 
                                     ! "ieee_logb failed for PINF_4

	if (xres(6) /= exponent(values(6))-1 ) error stop 6 
		                     ! "ieee_logb failed for PHD_4
	    
	if (xres(7) /= exponent(values(7))-1 ) error stop 7 
                                     ! "ieee_logb failed for PTD_4

	     
	if (ires(8) /= iNINF_4) error stop 8 
                                     ! "ieee_logb failed for PZERO_4

	if (ires(9) /= iNINF_4) error stop 9 
                                     ! "ieee_logb failed for NZERO_4 
		
	if ( xres(10) /= exponent(values(10))-1 ) error stop 9 
                                     ! "ieee_logb failed for NTD_4

	if ( xres(11) /= exponent(values(11))-1 ) error stop 10 
                                     ! "ieee_logb failed for NHD_4

	if (ires(12) /= iPINF_4) error stop 11 
                                     ! "ieee_logb failed for NINF_4
	 

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) error stop 77
        end do


!Test array of kind 8

        call ieee_set_flag(ieee_all,.false.)

        xres_8 = ieee_logb(values_8)
		
	if (ires_8(1) /= iPNANQ_8) error stop 12 
                                     ! "ieee_logb failed for PNANS_8

	if (ires_8(2) /= iNNANQ_8) error stop 13 
                                     ! "ieee_logb failed for NNAN_8

	if (ires_8(3) /= iPNANQ_8) error stop 14 
                                     ! "ieee_logb failed for PNANQ_8

	if (ires_8(4) /= iNNANQ_8) error stop 15 
                                     ! "ieee_logb failed for NNANQ_8

	if (ires_8(5) /= iPINF_8) error stop 16 
                                     ! "ieee_logb failed for PINF_8

	if (xres_8(6) /= exponent(values_8(6))-1) error stop 17 
                                     ! "ieee_logb failed for PHD_8

	if (xres_8(7) /= exponent(values_8(7))-1) error stop 18 
                                     ! "ieee_logb failed for PTD_8

	if (ires_8(8) /= iNINF_8) error stop 19 
                                     ! "ieee_logb failed for PZERO_8

	if (ires_8(9) /= iNINF_8) error stop 20 
                                     ! "ieee_logb failed for NZERO_8

	if (xres_8(10) /= exponent(values_8(10))-1) error stop 21 
                                     ! "ieee_logb failed for NTD_8

	if (xres_8(11) /= exponent(values_8(11))-1) error stop 22 
                                     ! "ieee_logb failed for NHD_8

		  
	if (ires_8(12) /= iPINF_8) error stop 23 
                                     ! "ieee_logb failed for NINF_8
	    

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) error stop 88
        end do



!Test array of kind 16

        call ieee_set_flag(ieee_all,.false.)

        xres_16 = ieee_logb(values_16)
		
        if (apns(1) /= iPNANQ_16(1)) error stop 24 
                                     ! "ieee_logb failed for PNANS_16
        
	if (anns(1) /= iNNANQ_16(1)) error stop 25 
                                     ! "ieee_logb failed for NNANS_16
	
	if (apnq(1) /= iPNANQ_16(1)) error stop 26 
                                     ! "ieee_logb failed for PNANQ_16
	
	if (annq(1) /= iNNANQ_16(1)) error stop 27 
                                     ! "ieee_logb failed for NNANQ_16
	
	if (xres_16(5) /= PINF_16) error stop 28 
                                     ! "ieee_logb failed for PINF_16
	     
	if (xres_16(6) /= NINF_16)  error stop 29 
                                     ! "ieee_logb failed for PZERO_16
		 
	if (xres_16(7) /= NINF_16) error stop 30 
                                     ! "ieee_logb failed for PZERO2_16
		
	if (xres_16(8) /= PINF_16)  error stop 31 
                                     ! "ieee_logb failed for NINF_16
		 	 

        call ieee_get_flag(ieee_all,flag_values)
        do i =1,5
           if (expect_value(i).neqv.flag_values(i)) error stop 99
        end do

	end program

