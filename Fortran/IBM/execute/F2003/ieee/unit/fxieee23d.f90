! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee23d
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
!*  DATE                       : March, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_NEXT_AFTER
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
        program fxieee23
        use ieee_arithmetic
        use constants_for_ieee

        real(8), parameter, dimension(12) :: values = &
     &       (/             &
     &       PINF_8,        & ! Positive INF
     &       huge(PINF_8),  & ! Positive Normal
     &       tiny(PINF_8),  & ! Positive Normal
     &       PHD_8,         & ! Positive Denormal
     &       PTD_8,         & ! Positive Denormal
     &       PZERO_8,       & ! Positive Zero
     &       NZERO_8,       & ! Negative Zero
     &       NTD_8,         & ! Negative Denormal
     &       NHD_8,         & ! Negative Denormal
     &       -tiny(PINF_8), & ! Negative Normal
     &       -huge(PINF_8), & ! Negative Normal
     &       NINF_8         & ! Negative INF
     &       /)
        real(8), parameter, dimension(12) :: values2 = values

        real(8), dimension(12) :: results

        real(8) :: result
        integer(8) :: iresult
        equivalence(result,iresult)

        logical(4), dimension(5) :: flag_values
        logical(4), dimension(5) :: expected_flags

        integer :: i


        ! Test 1: When x == y, the result shall be x
        !         and no flags shall be set

        ! Ensure all exception flags are quiet
        call ieee_set_flag(ieee_all,.false.)

        ! For each ieee class, test that when x .eq. y, the result is x
        ! and no exception flag will be set
        results = ieee_next_after(values, values2)

        call ieee_get_flag(ieee_all, flag_values)
        expected_flags = (/.false., .false., .false., .false., .false./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "Failed.  Exception flags were set."
          endif
          if (results(i) .ne. values(i)) then
            print *, "Failed.  Result(",i,") not the same as the input."
          endif
        enddo


        ! Test 2: x = 0.0, y = -0.0 and vice versa.
        !         Result shall be x and no exception flags shall be set.
        call ieee_set_flag(ieee_all, .false.)

        result = ieee_next_after(PZERO_8,NZERO_8)
        if (iresult .ne. iPZERO_8) then
          print *, "testzeros value error 1"
        endif

        result = ieee_next_after(NZERO_8,PZERO_8)
        if (iresult .ne. iNZERO_8) then
          print *, "testzeros value error 2"
        endif

        call ieee_get_flag(ieee_all, flag_values)
        do i = 1, 5
          if (flag_values(i) .neqv. .false.) then
            print *, "testzeros flag error 3"
          endif
        enddo


        ! Test 3: Testing when x /= y where y > x


        ! Huge(X) up.  result should equal INF and flags OI should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(huge(values),PINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                    OV      DBZ      INV      UN       IN
        expected_flags = (/.true., .false., .false., .false., .true./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 1"
          endif
        enddo
        if (result .ne. PINF_8) then
          print *, "error 2"
        endif

        ! HD up.  result should equal TINY and no flags should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(PHD_8,PINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV      UN       IN
        expected_flags = (/.false., .false., .false., .false., .false./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 3"
          endif
        enddo
        if (result .ne. tiny(PHD_8)) then
          print *, "error 4"
        endif


        ! TD up.  result should equal 2TD and flags UI should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(PTD_8,PINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV      UN      IN
        expected_flags = (/.false., .false., .false., .true., .true./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 5"
          endif
        enddo
        if (result .ne. (2.0_8 * PTD_8)) then
          print *, "error 6"
        endif


        ! 0.0 up.  result should equal TD and flags UI should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(PZERO_8,PINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV      UN      IN
        expected_flags = (/.false., .false., .false., .true., .true./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 7"
          endif
        enddo
        if (result .ne. PTD_8) then
          print *, "error 8"
        endif


        ! -0.0 up.  result should equal TD and flags UI should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(NZERO_8,PINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV      UN      IN
        expected_flags = (/.false., .false., .false., .true., .true./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 9"
          endif
        enddo
        if (result .ne. PTD_8) then
          print *, "error 10"
        endif


        ! -TD up.  result should equal -0.0 and flags UI should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(NTD_8,PINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV      UN      IN
        expected_flags = (/.false., .false., .false., .true., .true./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 11"
          endif
        enddo
        if (iresult .ne. iNZERO_8) then
          print *, "error 12"
        endif


        ! The remaining classes will be tested in the x > y case


        ! Test 3: Testing when x /= y where y > x


        ! -0.0 down.  result should equal -TD and flags UI should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(NZERO_8,NINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV      UN      IN
        expected_flags = (/.false., .false., .false., .true., .true./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 13"
          endif
        enddo
        if (result .ne. NTD_8) then
          print *, "error 14"
        endif


        ! 0.0 down.  result should equal -TD and flags UI should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(PZERO_8,NINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV      UN      IN
        expected_flags = (/.false., .false., .false., .true., .true./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 15"
          endif
        enddo
        if (result .ne. NTD_8) then
          print *, "error 16"
        endif


        ! TD down.  result should equal 0.0 and flags UI should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(PTD_8,NINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV      UN      IN
        expected_flags = (/.false., .false., .false., .true., .true./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 17"
          endif
        enddo
        if (iresult .ne. iPZERO_8) then
          print *, "error 18"
        endif


        ! TINY down.  result should equal HD and flags UI should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(tiny(NINF_8),NINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV      UN      IN
        expected_flags = (/.false., .false., .false., .true., .true./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 19"
          endif
        enddo
        if (result .ne. PHD_8) then
          print *, "error 20"
        endif


        ! HUGE down.  result should equal HUGE-Epsilon and no flags should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(huge(NINF_8),NINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV       UN       IN
        expected_flags = (/.false., .false., .false., .false., .false./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 21"
          endif
        enddo
        if (iresult .ne. z"7feffffffffffffe") then
          print *, "error 22"
        endif


        ! INF down.  result should equal HUGE and no flags should be set
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(PINF_8,NINF_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV       UN      IN
        expected_flags = (/.false., .false., .false., .false., .false./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 23"
          endif
        enddo
        if (result .ne. huge(PINF_8)) then
          print *, "error 24"
        endif


        ! Test 4:  One of X and Y is a NaNQ.  We should return one of the
        !          NaNQ's and no exceptions will be signaled


        ! x=NaNQ, y=0.0
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(PNANQ_8,PZERO_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV       UN      IN
        expected_flags = (/.false., .false., .false., .false., .false./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 25"
          endif
        enddo
        if (iresult .ne. iPNANQ_8) then
          print *, "error 26"
        endif


        ! x=0.0, y=NaNQ
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(PZERO_8,NNANQ_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV       UN      IN
        expected_flags = (/.false., .false., .false., .false., .false./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 27"
          endif
        enddo
        if (iresult .ne. iNNANQ_8) then
          print *, "error 28"
        endif

        ! Test 5:  One of X and Y is a NaNS.  We should return the
        !          corresponding NaNQ and signal invalid


        ! x=NaNS, y=0.0
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(PNANS_8,PZERO_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV       UN      IN
        expected_flags = (/.false., .false., .true., .false., .false./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 29"
          endif
        enddo
        if (iresult .ne. iPNANQ_8) then
          print *, "error 30"
        endif


        ! x=0.0, y=NaNS
        call ieee_set_flag(ieee_all,.false.)
        result = ieee_next_after(PZERO_8,NNANS_8)
        call ieee_get_flag(ieee_all, flag_values)
        !                     OV      DBZ      INV       UN      IN
        expected_flags = (/.false., .false., .true., .false., .false./)
        do i = 1, 5
          if (flag_values(i) .neqv. expected_flags(i)) then
            print *, "error 31"
          endif
        enddo
        if (iresult .ne. iNNANQ_8) then
          print *, "error 32"
        endif

        end

