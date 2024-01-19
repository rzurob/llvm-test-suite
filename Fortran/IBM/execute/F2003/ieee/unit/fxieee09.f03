 ! *********************************************************************
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 6, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NORMAL
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qintsize
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
        program fxieee09

        use ieee_arithmetic
        use constants_for_ieee

        real(4), dimension(4) :: args4
        real(8), dimension(4) :: args8
        real(16), dimension(4) :: args16
        logical :: results(4), flag_values(5)
        integer :: i

        ! ieee_is_normal should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.
        call ieee_set_flag(ieee_all,.false.)

! Test operation for real(4)'s

        if (ieee_is_normal(PINF_4)) then
           print *, "ieee_is_normal(PINF_4) failed."
        endif

        if (ieee_is_normal(NINF_4)) then
            print *, "ieee_is_normal(NINF_4) failed."
        endif

        if (ieee_is_normal(huge(PINF_4)) .neqv. .true.) then
            print *, "ieee_is_normal(HUGE) failed."
        endif

        if (ieee_is_normal(tiny(PINF_4)) .neqv. .true.) then
            print *, "ieee_is_normal(TINY) failed."
        endif

        if (ieee_is_normal(PHD_4)) then
            print *, "ieee_is_normal(PHD_4) failed."
        endif

        if (ieee_is_normal(PTD_4)) then
            print *, "ieee_is_normal(PTD_4) failed."
        endif

        if (ieee_is_normal(NHD_4)) then
            print *, "ieee_is_normal(NHD_4) failed."
        endif

        if (ieee_is_normal(NTD_4)) then
            print *, "ieee_is_normal(NTD_4) failed."
        endif

        if (ieee_is_normal(PZERO_4) .neqv. .true.) then
            print *, "ieee_is_normal(PZERO_4) failed."
        endif

        if (ieee_is_normal(NZERO_4) .neqv. .true.) then
            print *, "ieee_is_normal(NZERO_4) failed."
        endif

        if (ieee_is_normal(-tiny(PINF_4)) .neqv. .true.) then
            print *, "ieee_is_normal(-TINY) failed."
        endif

        if (ieee_is_normal(-huge(PINF_4)) .neqv. .true.) then
            print *, "ieee_is_normal(-HUGE) failed."
        endif

        args4 = (/ PNANQ_4, PNANS_4, NNANQ_4, NNANS_4 /)
        results = ieee_is_normal(args4)
        if (results(1)) then
           print *, "ieee_is_normal(array) failed for PNANQ_4."
        endif
        if (results(2)) then
           print *, "ieee_is_normal(array) failed for PNANS_4."
        endif
        if (results(3)) then
           print *, "ieee_is_normal(array) failed for NNANQ_4."
        endif
        if (results(4)) then
           print *, "ieee_is_normal(array) failed for NNANS_4."
        endif

! Test Operation for real(8)'s

        if (ieee_is_normal(PINF_8)) then
           print *, "ieee_is_normal(PINF_8) failed."
        endif

        if (ieee_is_normal(NINF_8)) then
            print *, "ieee_is_normal(NINF_8) failed."
        endif

        if (ieee_is_normal(huge(PINF_8)) .neqv. .true.) then
            print *, "ieee_is_normal(HUGE) failed."
        endif

        if (ieee_is_normal(tiny(PINF_8)) .neqv. .true.) then
            print *, "ieee_is_normal(TINY) failed."
        endif

        if (ieee_is_normal(PHD_8)) then
            print *, "ieee_is_normal(PHD_8) failed."
        endif

        if (ieee_is_normal(PTD_8)) then
            print *, "ieee_is_normal(PTD_8) failed."
        endif

        if (ieee_is_normal(NHD_8)) then
            print *, "ieee_is_normal(NHD_8) failed."
        endif

        if (ieee_is_normal(NTD_8)) then
            print *, "ieee_is_normal(NTD_8) failed."
        endif

        if (ieee_is_normal(PZERO_8) .neqv. .true.) then
            print *, "ieee_is_normal(PZERO_8) failed."
        endif

        if (ieee_is_normal(NZERO_8) .neqv. .true.) then
            print *, "ieee_is_normal(NZERO_8) failed."
        endif

        if (ieee_is_normal(-tiny(PINF_8)) .neqv. .true.) then
            print *, "ieee_is_normal(-TINY) failed."
        endif

        if (ieee_is_normal(-huge(PINF_8)) .neqv. .true.) then
            print *, "ieee_is_normal(-HUGE) failed."
        endif

        args8 = (/PNANQ_8, PNANS_8, NNANQ_8, NNANS_8 /)
        results = ieee_is_normal(args8)
        if (results(1)) then
           print *, "ieee_is_normal(array) failed for PNANQ_8."
        endif
        if (results(2)) then
           print *, "ieee_is_normal(array) failed for PNANS_8."
        endif
        if (results(3)) then
           print *, "ieee_is_normal(array) failed for NNANQ_8."
        endif
        if (results(4)) then
           print *, "ieee_is_normal(array) failed for NNANS_8."
        endif

! Test Operation for real(16)'s

        if (ieee_is_normal(2.0_16) .eqv. .false. ) then
           print *, "ieee_is_normal(PINF_16) failed."
        endif

        if (ieee_is_normal(NINF_16)) then
           print *, "ieee_is_normal(NINF_16) failed."
        endif

        if (ieee_is_normal(huge(PINF_16)) .neqv. .true.) then
           print *, "ieee_is_normal(HUGE) failed."
        endif

        if (ieee_is_normal(tiny(PINF_16)) .neqv. .true.) then
           print *, "ieee_is_normal(TINY) failed."
        endif

        if (ieee_is_normal(PZERO_16) .neqv. .true.) then
           print *, "ieee_is_normal(PZERO_16) failed."
        endif

        if (ieee_is_normal(PZERO2_16) .neqv. .true.) then
           print *, "ieee_is_normal(PZERO_16) failed."
        endif

        if (ieee_is_normal(-tiny(PINF_16)) .neqv. .true.) then
           print *, "ieee_is_normal(-TINY) failed."
        endif

        if (ieee_is_normal(-huge(PINF_16)) .neqv. .true.) then
           print *, "ieee_is_normal(-HUGE) failed."
        endif

        args16 = (/PNANQ_16, PNANS_16, NNANQ_16, NNANS_16 /)
        results = ieee_is_normal(args16)
        if (results(1)) then
           print *, "ieee_is_normal(array) failed for PNANQ_16."
        endif
        if (results(2)) then
           print *, "ieee_is_normal(array) failed for PNANS_16."
        endif
        if (results(3)) then
           print *, "ieee_is_normal(array) failed for NNANQ_16."
        endif
        if (results(4)) then
           print *, "ieee_is_normal(array) failed for NNANS_16."
        endif


       !   for positive and negative denormal not yet
       !   ......


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.) then
                print *, "ieee_is_normal failed: An exception flag (",i,") was set."
            endif
        enddo

        end program
