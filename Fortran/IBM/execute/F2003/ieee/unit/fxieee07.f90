! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee07
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NAN
!*  SECONDARY FUNCTIONS TESTED :
!*
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : -qintsize
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
        program fxieee07

        use ieee_arithmetic
        use constants_for_ieee

        real(4), dimension(4) :: args4
        real(8), dimension(4) :: args8
        real(16), dimension(4) :: args16
        logical :: results(4), flag_values(5)
        integer :: i

        ! ieee_is_nan should not set any flags.  Clear all flags and
        ! check at the end that all flags are clear.
        call ieee_set_flag(ieee_all,.false.)

! Test operation for real(4)'s

        if (ieee_support_nan(PINF_4)) then
           if (ieee_is_nan(PINF_4)) then
              print *, "ieee_is_nan(PINF_4) failed."
           endif
        endif

        if (ieee_is_nan(NINF_4)) then
            print *, "ieee_is_nan(PINF_4) failed."
        endif

        if (ieee_is_nan(huge(PINF_4))) then
            print *, "ieee_is_nan(HUGE) failed."
        endif

        if (ieee_is_nan(tiny(PINF_4))) then
            print *, "ieee_is_nan(TINY) failed."
        endif

        if (ieee_is_nan(PHD_4)) then
            print *, "ieee_is_nan(PHD_4) failed."
        endif

        if (ieee_is_nan(PTD_4)) then
            print *, "ieee_is_nan(PTD_4) failed."
        endif

        if (ieee_is_nan(NHD_4)) then
            print *, "ieee_is_nan(NHD_4) failed."
        endif

        if (ieee_is_nan(NTD_4)) then
            print *, "ieee_is_nan(NTD_4) failed."
        endif

        if (ieee_is_nan(PZERO_4)) then
            print *, "ieee_is_nan(PZERO_4) failed."
        endif

        if (ieee_is_nan(NZERO_4) ) then
            print *, "ieee_is_nan(NZERO_4) failed."
        endif

        if (ieee_is_nan(-tiny(PINF_4))) then
            print *, "ieee_is_nan(-TINY) failed."
        endif

        if (ieee_is_nan(-huge(PINF_4)) ) then
            print *, "ieee_is_nan(-HUGE) failed."
        endif

        args4 = (/ PNANQ_4, PNANS_4, NNANQ_4, NNANS_4 /)
        if (ieee_support_nan(args4) ) then
        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.) then
                print *, "ieee_is_nan failed for real*4: An exception flag (",i,") was set."
            endif
        enddo
           results = ieee_is_nan(args4)
        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.) then
                print *, "ieee_is_nan failed for real*4: An exception flag (",i,") was set."
            endif
        enddo
           if (results(1) .neqv. .true.) then
              print *, "ieee_is_nan(array) failed for NNANQ_4."
           endif
           if (results(2) .neqv. .true.) then
              print *, "ieee_is_nan(array) failed for NNANS_4."
           endif
           if (results(3) .neqv. .true.) then
              print *, "ieee_is_nan(array) failed for NNANQ_4."
           endif
           if (results(4) .neqv. .true.) then
              print *, "ieee_is_nan(array) failed for NNANS_4."
           endif
        endif
        
        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.) then
                print *, "ieee_is_nan failed for real*4: An exception flag (",i,") was set."
            endif
        enddo
        call ieee_set_flag(ieee_all,.false.)

! Test Operation for real(8)'s

        if (ieee_support_nan(PINF_8)) then
           if (ieee_is_nan(PINF_8)) then
              print *, "ieee_is_nan(PINF_8) failed."
           endif
        endif

        if (ieee_is_nan(NINF_8)) then
            print *, "ieee_is_nan(PINF_8) failed."
        endif

        if (ieee_is_nan(huge(PINF_8))) then
            print *, "ieee_is_nan(HUGE) failed."
        endif

        if (ieee_is_nan(tiny(PINF_8)) ) then
            print *, "ieee_is_nan(TINY) failed."
        endif

        if (ieee_is_nan(PHD_8) ) then
            print *, "ieee_is_nan(PHD_8) failed."
        endif

        if (ieee_is_nan(PTD_8) ) then
            print *, "ieee_is_nan(PTD_8) failed."
        endif

        if (ieee_is_nan(NHD_8) ) then
            print *, "ieee_is_nan(NHD_8) failed."
        endif

        if (ieee_is_nan(NTD_8) ) then
            print *, "ieee_is_nan(NTD_8) failed."
        endif

        if (ieee_is_nan(PZERO_8) ) then
            print *, "ieee_is_nan(PZERO_8) failed."
        endif

        if (ieee_is_nan(NZERO_8)) then
            print *, "ieee_is_nan(NZERO_8) failed."
        endif

        if (ieee_is_nan(-tiny(PINF_8)) ) then
            print *, "ieee_is_nan(-TINY) failed."
        endif

        if (ieee_is_nan(-huge(PINF_8))  ) then
            print *, "ieee_is_nan(-HUGE) failed."
        endif

        args8 = (/PNANQ_8, PNANS_8, NNANQ_8, NNANS_8 /)
        if (ieee_support_nan(args8)) then
           results = ieee_is_nan(args8)
           if (results(1) .neqv. .true.) then
              print *, "ieee_is_nan(array) failed for NNANQ_8."
           endif
           if (results(2) .neqv. .true.) then
              print *, "ieee_is_nan(array) failed for NNANS_8."
           endif
           if (results(3) .neqv. .true.) then
              print *, "ieee_is_nan(array) failed for NNANQ_8."
           endif
           if (results(4) .neqv. .true.) then
              print *, "ieee_is_nan(array) failed for NNANS_8."
           endif
        end if
        
        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.) then
                print *, "ieee_is_nan failed for real*8: An exception flag (",i,") was set."
            endif
        enddo
        call ieee_set_flag(ieee_all,.false.)


! Test Operation for real(16)'s

         if (ieee_is_nan(PINF_16)) then
            print *, "ieee_is_nan(PINF_16) failed."
         endif

         if (ieee_is_nan(NINF_16)) then
            print *, "ieee_is_nan(PINF_16) failed."
         endif

         if (ieee_is_nan(huge(PINF_16)) ) then
            print *, "ieee_is_nan(HUGE) failed."
         endif

         if (ieee_is_nan(tiny(PINF_16)) ) then
            print *, "ieee_is_nan(TINY) failed."
         endif

         if (ieee_is_nan(PZERO_16) ) then
            print *, "ieee_is_nan(PZERO_16) failed."
         endif

         if (ieee_is_nan(PZERO2_16) ) then
            print *, "ieee_is_nan(PZERO_16) failed."
         endif

         if (ieee_is_nan(-tiny(PINF_16)) ) then
            print *, "ieee_is_nan(-TINY) failed."
         endif

         if (ieee_is_nan(-huge(PINF_16))  ) then
            print *, "ieee_is_nan(-HUGE) failed."
         endif

         args16 = (/PNANQ_16, PNANS_16, NNANQ_16, NNANS_16 /)
         results = ieee_is_nan(args16)
         if (results(1) .neqv. .true.) then
            print *, "ieee_is_nan(array) failed for NNANQ_16."
         endif
         if (results(2) .neqv. .true.) then
            print *, "ieee_is_nan(array) failed for NNANS_16."
         endif
         if (results(3) .neqv. .true.) then
            print *, "ieee_is_nan(array) failed for NNANQ_16."
         endif
         if (results(4) .neqv. .true.) then
            print *, "ieee_is_nan(array) failed for NNANS_16."
         endif


       !   for positive and negative denormal not yet
       !   ......


        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.) then
                print *, "ieee_is_nan failed for real*16: An exception flag (",i,") was set."
            endif
        enddo

        end program
