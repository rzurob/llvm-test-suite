! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee08 
! %COMPOPTS: -qfree=f90
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
!*  PROGRAMMER                 : Marcus Yu
!*  DATE                       : February 6, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NEGATIVE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
        program fxieee08

        use ieee_arithmetic
        use constants_for_ieee

        real(4), dimension(4) :: args4
        real(8), dimension(4) :: args8
        real(16), dimension(4) :: args16
        logical :: results(4), flag_values(5)
        integer :: i

        ! ieee_is_negative should not set any flags. Clear all flags and
        ! check at the end that all flags are clear.
        call ieee_set_flag(ieee_all,.false.)

! Test operation for real(4)'s

        if (ieee_is_negative(PINF_4)) then
           print *, "ieee_is_negative(PINF_4) failed."
        endif

        if (ieee_is_negative(NINF_4) .neqv. .true.) then
            print *, "ieee_is_negative(NINF_4) failed."
        endif

        if (ieee_is_negative(huge(PINF_4))) then
            print *, "ieee_is_negative(HUGE) failed."
        endif

        if (ieee_is_negative(tiny(PINF_4))) then
            print *, "ieee_is_negative(TINY) failed."
        endif

        if (ieee_is_negative(PHD_4)) then
            print *, "ieee_is_negative(PHD_4) failed."
        endif

        if (ieee_is_negative(PTD_4)) then
            print *, "ieee_is_negative(PTD_4) failed."
        endif

        if (ieee_is_negative(NHD_4) .neqv. .true.) then
            print *, "ieee_is_negative(NHD_4) failed."
        endif

        if (ieee_is_negative(NTD_4) .neqv. .true.) then
            print *, "ieee_is_negative(NTD_4) failed."
        endif

        if (ieee_is_negative(PZERO_4)) then
            print *, "ieee_is_negative(PZERO_4) failed."
        endif

        if (ieee_is_negative(NZERO_4) .neqv. .true.) then
            print *, "ieee_is_negative(NZERO_4) failed."
        endif

        if (ieee_is_negative(-tiny(PINF_4)) .neqv. .true.) then
            print *, "ieee_is_negative(-TINY) failed."
        endif

        if (ieee_is_negative(-huge(PINF_4)) .neqv. .true.) then
            print *, "ieee_is_negative(-HUGE) failed."
        endif

        args4 = (/ PNANQ_4, PNANS_4, NNANQ_4, NNANS_4 /)
        results = ieee_is_negative(args4)
        if (results(1)) then
           print *, "ieee_is_negative(array) failed for PNANQ_4."
        endif
        if (results(2)) then
           print *, "ieee_is_negative(array) failed for PNANS_4."
        endif
        if (results(3)) then
           print *, "ieee_is_negative(array) failed for NNANQ_4."
        endif
        if (results(4)) then
           print *, "ieee_is_negative(array) failed for NNANS_4."
        endif

! Test Operation for real(8)'s

        if (ieee_is_negative(PINF_8)) then
           print *, "ieee_is_negative(PINF_8) failed."
        endif

        if (ieee_is_negative(NINF_8) .neqv. .true.) then
            print *, "ieee_is_negative(NINF_8) failed."
        endif

        if (ieee_is_negative(huge(PINF_8))) then
            print *, "ieee_is_negative(HUGE) failed."
        endif

        if (ieee_is_negative(tiny(PINF_8)) ) then
            print *, "ieee_is_negative(TINY) failed."
        endif

        if (ieee_is_negative(PHD_8)) then
            print *, "ieee_is_negative(PHD_8) failed."
        endif

        if (ieee_is_negative(PTD_8)) then
            print *, "ieee_is_negative(PTD_8) failed."
        endif

        if (ieee_is_negative(NHD_8) .neqv. .true.) then
            print *, "ieee_is_negative(NHD_8) failed."
        endif

        if (ieee_is_negative(NTD_8) .neqv. .true.) then
            print *, "ieee_is_negative(NTD_8) failed."
        endif

        if (ieee_is_negative(PZERO_8) ) then
            print *, "ieee_is_negative(PZERO_8) failed."
        endif

        if (ieee_is_negative(NZERO_8) .neqv. .true.) then
            print *, "ieee_is_negative(NZERO_8) failed."
        endif

        if (ieee_is_negative(-tiny(PINF_8)) .neqv. .true.) then
            print *, "ieee_is_negative(-TINY) failed."
        endif

        if (ieee_is_negative(-huge(PINF_8)) .neqv. .true.) then
            print *, "ieee_is_negative(-HUGE) failed."
        endif

        args8 = (/PNANQ_8, PNANS_8, NNANQ_8, NNANS_8 /)
        results = ieee_is_negative(args8)
        if (results(1)) then
           print *, "ieee_is_negative(array) failed for PNANQ_8."
        endif
        if (results(2)) then
           print *, "ieee_is_negative(array) failed for PNANS_8."
        endif
        if (results(3)) then
           print *, "ieee_is_negative(array) failed for NNANQ_8."
        endif
        if (results(4)) then
           print *, "ieee_is_negative(array) failed for NNANS_8."
        endif

! Test Operation for real(16)'s

         if (ieee_is_negative(PINF_16)) then
            print *, "ieee_is_negative(PINF_16) failed."
         endif

         if (ieee_is_negative(NINF_16) .neqv. .true.) then
            print *, "ieee_is_negative(NINF_16) failed."
         endif

         if (ieee_is_negative(huge(PINF_16)) ) then
            print *, "ieee_is_negative(HUGE) failed."
         endif

         if (ieee_is_negative(tiny(PINF_16)) ) then
            print *, "ieee_is_negative(TINY) failed."
         endif

         if (ieee_is_negative(PZERO_16) ) then
            print *, "ieee_is_negative(PZERO_16) failed."
         endif

         if (ieee_is_negative(PZERO2_16) .neqv. .true. ) then
            print *, "ieee_is_negative(PZERO_16) failed."
         endif

         if (ieee_is_negative(-tiny(PINF_16)) .neqv. .true.) then
            print *, "ieee_is_negative(-TINY) failed."
         endif

         if (ieee_is_negative(-huge(PINF_16)) .neqv. .true.) then
            print *, "ieee_is_negative(-HUGE) failed."
         endif

         args16 = (/PNANQ_16, PNANS_16, NNANQ_16, NNANS_16 /)
         results = ieee_is_negative(args16)
         if (results(1)) then
            print *, "ieee_is_negative(array) failed for PNANQ_16."
         endif
         if (results(2)) then
            print *, "ieee_is_negative(array) failed for PNANS_16."
         endif
         if (results(3)) then
            print *, "ieee_is_negative(array) failed for NNANQ_16."
         endif
         if (results(4)) then
            print *, "ieee_is_negative(array) failed for NNANS_16."
         endif


       !   for positive and negative denormal not yet
       !   ......


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.) then
                print *, "ieee_is_negative failed: An exception flag (",i,") was set."
            endif
        enddo

        end program
