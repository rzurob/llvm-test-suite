! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee06
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
!*
!*  DATE                       : February 6, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_FINITE
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
        program fxieee06

        use ieee_arithmetic
        use constants_for_ieee

        real(4), dimension(4) :: args4
        real(8), dimension(4) :: args8
        real(16), dimension(4) :: args16
        logical :: results(4), flag_values(5)
        integer :: i

        ! ieee_is_finite should not set any flags.  Clear all flags and
        ! check at the end that all flags are clear.
        call ieee_set_flag(ieee_all,.false.)

! Test operation for real(4)'s

        if (ieee_support_datatype(PINF_4)) then
           if (ieee_is_finite(PINF_4)) then
              print *, "ieee_is_finite(PINF_4) failed."
           endif
        endif

        if (ieee_support_datatype(NINF_4)) then
           if (ieee_is_finite(NINF_4)) then
              print *, "ieee_is_finite(PINF_4) failed."
           endif
        endif

        if (ieee_support_datatype(huge(PINF_4))) then
           if (ieee_is_finite(huge(PINF_4)) .neqv. .true.) then
              print *, "ieee_is_finite(HUGE) failed."
           endif
        endif

        if (ieee_support_datatype(tiny(PINF_4))) then
           if (ieee_is_finite(tiny(PINF_4)) .neqv. .true.) then
              print *, "ieee_is_finite(TINY) failed."
           endif
        endif

        if (ieee_support_datatype(PHD_4)) then
           if (ieee_is_finite(PHD_4) .neqv. .true.) then
              print *, "ieee_is_finite(PHD_4) failed."
           endif
        endif

        if (ieee_support_datatype(PTD_4)) then
           if (ieee_is_finite(PTD_4) .neqv. .true.) then
              print *, "ieee_is_finite(PTD_4) failed."
           endif
        endif

        if (ieee_support_datatype(NHD_4)) then
           if (ieee_is_finite(NHD_4) .neqv. .true.) then
              print *, "ieee_is_finite(NHD_4) failed."
           endif
        endif

        if (ieee_support_datatype(NTD_4)) then
           if (ieee_is_finite(NTD_4) .neqv. .true.) then
              print *, "ieee_is_finite(NTD_4) failed."
           endif
        endif

        if (ieee_support_datatype(PZERO_4)) then
           if (ieee_is_finite(PZERO_4) .neqv. .true.) then
              print *, "ieee_is_finite(PZERO_4) failed."
           endif
        endif

        if (ieee_support_datatype(NZERO_4)) then
           if (ieee_is_finite(NZERO_4) .neqv. .true. ) then
              print *, "ieee_is_finite(NZERO_4) failed."
           endif
        endif

        if (ieee_support_datatype(-tiny(PINF_4))) then
           if (ieee_is_finite(-tiny(PINF_4)) .neqv. .true.) then
              print *, "ieee_is_finite(-TINY) failed."
           endif
        endif

        if (ieee_support_datatype(-huge(PINF_4))) then
           if (ieee_is_finite(-huge(PINF_4)) .neqv. .true. ) then
              print *, "ieee_is_finite(-HUGE) failed."
           endif
        endif

        args4 = (/ PNANQ_4, PNANS_4, NNANQ_4, NNANS_4 /)
        if (ieee_support_datatype(args4)) then
           results = ieee_is_finite(args4)
           if (results(1)) then
              print *, "ieee_is_finite(array) failed for NNANQ_4."
           endif
           if (results(2)) then
              print *, "ieee_is_finite(array) failed for NNANS_4."
           endif
           if (results(3)) then
              print *, "ieee_is_finite(array) failed for NNANQ_4."
           endif
           if (results(4)) then
              print *, "ieee_is_finite(array) failed for NNANS_4."
           endif
        endif

! Test Operation for real(8)'s

        if (ieee_support_datatype(PINF_8)) then
           if (ieee_is_finite(PINF_8)) then
              print *, "ieee_is_finite(PINF_8) failed."
           endif
        endif

        if (ieee_support_datatype(NINF_8)) then
           if (ieee_is_finite(NINF_8)) then
              print *, "ieee_is_finite(PINF_8) failed."
           endif
        endif

        if (ieee_support_datatype(huge(PINF_8))) then
           if (ieee_is_finite(huge(PINF_8)) .neqv. .true.) then
              print *, "ieee_is_finite(HUGE) failed."
           endif
        endif

        if (ieee_support_datatype(tidy(PINF_8))) then
           if (ieee_is_finite(tiny(PINF_8)) .neqv. .true.) then
              print *, "ieee_is_finite(TINY) failed."
           endif
        endif

        if (ieee_support_datatype(PHD_8)) then
           if (ieee_is_finite(PHD_8) .neqv. .true.) then
              print *, "ieee_is_finite(PHD_8) failed."
           endif
        endif

        if (ieee_support_datatype(PTD_8)) then
           if (ieee_is_finite(PTD_8) .neqv. .true.) then
              print *, "ieee_is_finite(PTD_8) failed."
           endif
        endif

        if (ieee_support_datatype(NHD_8)) then
           if (ieee_is_finite(NHD_8) .neqv. .true.) then
              print *, "ieee_is_finite(NHD_8) failed."
           endif
        endif

        if (ieee_support_datatype(NTD_8)) then
           if (ieee_is_finite(NTD_8) .neqv. .true.) then
              print *, "ieee_is_finite(NTD_8) failed."
           endif
        endif

        if (ieee_support_datatype(PZERO_8)) then
           if (ieee_is_finite(PZERO_8) .neqv. .true.) then
              print *, "ieee_is_finite(PZERO_8) failed."
           endif
        endif

        if (ieee_support_datatype(NZERO_8)) then
           if (ieee_is_finite(NZERO_8) .neqv. .true. ) then
              print *, "ieee_is_finite(NZERO_8) failed."
           endif
        endif

        if (ieee_support_datatype(-tiny(PINF_8))) then
           if (ieee_is_finite(-tiny(PINF_8)) .neqv. .true.) then
              print *, "ieee_is_finite(-TINY) failed."
           endif
        endif

        if (ieee_support_datatype(-huge(PINF_8))) then
           if (ieee_is_finite(-huge(PINF_8)) .neqv. .true. ) then
              print *, "ieee_is_finite(-HUGE) failed."
           endif
        endif

        args8 = (/PNANQ_8, PNANS_8, NNANQ_8, NNANS_8 /)
        if (ieee_support_datatype(args8)) then
           results = ieee_is_finite(args8)
           if (results(1)) then
              print *, "ieee_is_finite(array) failed for NNANQ_8."
           endif
           if (results(2)) then
              print *, "ieee_is_finite(array) failed for NNANS_8."
           endif
           if (results(3)) then
              print *, "ieee_is_finite(array) failed for NNANQ_8."
           endif
           if (results(4)) then
              print *, "ieee_is_finite(array) failed for NNANS_8."
           endif
        end if

! Test Operation for real(16)'s

         if (ieee_is_finite(PINF_16)) then
           print *, "ieee_is_finite(PINF_16) failed."
         endif

         if (ieee_is_finite(NINF_16)) then
            print *, "ieee_is_finite(PINF_16) failed."
         endif

         if (ieee_is_finite(huge(PINF_16)) .neqv. .true.) then
            print *, "ieee_is_finite(HUGE) failed."
         endif

         if (ieee_is_finite(tiny(PINF_16)) .neqv. .true.) then
            print *, "ieee_is_finite(TINY) failed."
         endif

         if (ieee_is_finite(PZERO_16) .neqv. .true.) then
            print *, "ieee_is_finite(PZERO_16) failed."
         endif

         if (ieee_is_finite(PZERO2_16) .neqv. .true.) then
            print *, "ieee_is_finite(PZERO_16) failed."
         endif

         if (ieee_is_finite(-tiny(PINF_16)) .neqv. .true.) then
            print *, "ieee_is_finite(-TINY) failed."
         endif

         if (ieee_is_finite(-huge(PINF_16)) .neqv. .true. ) then
            print *, "ieee_is_finite(-HUGE) failed."
         endif

         args16 = (/PNANQ_16, PNANS_16, NNANQ_16, NNANS_16 /)
         results = ieee_is_finite(args16)
         if (results(1)) then
            print *, "ieee_is_finite(array) failed for NNANQ_16."
         endif
         if (results(2)) then
             print *, "ieee_is_finite(array) failed for NNANS_16."
         endif
         if (results(3)) then
            print *, "ieee_is_finite(array) failed for NNANQ_16."
         endif
         if (results(4)) then
           print *, "ieee_is_finite(array) failed for NNANS_16."
         endif


       !   for positive and negative denormal not yet
       !   ......

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.) then
                print *, "ieee_is_finite failed: An exception flag (",i,") was set."
            endif
        enddo

        end program
