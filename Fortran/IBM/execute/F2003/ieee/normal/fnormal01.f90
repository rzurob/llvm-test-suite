! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxi3e2.presh fnormal01
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NORMAL 
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
!*  DESCRIPTION                : Testing IEEE_IS_NORMAL for REAL(4).
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fnormal01
      
        use ieee_arithmetic
        use constants_for_ieee
       
        real(4), dimension(5) :: normal_result
        real(4), parameter :: normal_pos1 = tiny(1.0)
        real(4), parameter :: normal_pos2 = huge(1.0)
        real(4), dimension(4) :: arrval
        logical :: actual_results(4), actual_flag_values(5)
        integer :: caseid, k

        caseid = 1
!... Check  that all flags are false
!... if not, set to .false.

     call ieee_get_flag(ieee_all, actual_flag_values)
     do k = 1, 5
        if (actual_flag_values(k) .neqv. .false. ) then
           call ieee_set_flag(ieee_all(k), .false. )
        endif
     enddo
!...test with PINF and NINF values
        if (ieee_support_datatype(PINF_4) .AND. ieee_support_datatype(NINF_4)) then
           if (ieee_is_normal(PINF_4) .OR. ieee_is_normal(NINF_4)) then
              call zzrc(caseid) 
           endif
        endif

!...test with PHD, PTD, NHD, NTD values        
       if (ieee_support_datatype(PHD_4) .AND. ieee_support_datatype(PTD_4)) then
           if (ieee_is_normal(PHD_4) .eqv. .true. ) then
              call zzrc(caseid+1)
           endif
           if (ieee_is_normal(PTD_4) .eqv. .true. ) then
              call zzrc(caseid+2)
           endif
       endif
        
       if (ieee_support_datatype(NHD_4) .AND. ieee_support_datatype(NTD_4)) then
           if (ieee_is_normal(NHD_4) .eqv. .true.) then
              call zzrc(caseid+3)
           endif
           if (ieee_is_normal(NTD_4) .eqv. .true.) then
              call zzrc(caseid+4)
           endif
       endif

!...test with PZERO and NZERO values       
        if (ieee_support_datatype(PZERO_4) .AND. ieee_support_datatype(NZERO_4)) then
           if (ieee_is_normal(PZERO_4) .neqv. .true.) then
              call zzrc(caseid+5)
           endif
           if (ieee_is_normal(NZERO_4) .neqv. .true. ) then
              call zzrc(caseid+6)
           endif
        endif

!...test with normal values resulted from tiny and huge
        if (ieee_support_datatype(normal_pos1)) then
           if (ieee_is_normal(normal_pos1) .neqv. .true. ) then
              call zzrc(caseid+7)
           endif
        endif

       if (ieee_support_datatype(normal_pos2)) then    
           if (ieee_is_normal(normal_pos2) .neqv. .true.) then
              call zzrc(caseid+8)
           endif
       endif
        
!...test with arrays
        arrval = (/ PNANQ_4, PNANS_4, NNANQ_4, NNANS_4 /)
        if (ieee_support_datatype(arrval)) then
           actual_results = ieee_is_normal(arrval)
           if (actual_results(1) .neqv. .false.) then
              call zzrc(caseid+9)
           endif
           if (actual_results(2) .neqv. .false.) then
              call zzrc(caseid+10)
           endif
           if (actual_results(3) .neqv. .false.) then
              call zzrc(caseid+11)
           endif
           if (actual_results(4) .neqv. .false.) then
              call zzrc(caseid+12)
           endif
        endif

!...test IEEE_IS_NORMAL with normal values resulting from operations
        normal_result(1) = PNORMAL1_4/PNORMAL2_4
        if (ieee_support_datatype(normal_result)) then
           if (ieee_is_normal(normal_result(1)) .eqv. .false. ) then
              call zzrc(caseid+13)
           endif
        endif

        normal_result(2) = NNORMAL1_4/PNORMAL2_4
        if (ieee_support_datatype(normal_result)) then
           if (ieee_is_normal(normal_result(2)) .eqv. .false. ) then
              call zzrc(caseid+14)
           endif
        endif

        normal_result(3) = NNORMAL1_4/NNORMAL2_4
        if (ieee_support_datatype(normal_result)) then
           if (ieee_is_normal(normal_result(3)) .eqv. .false. ) then
              call zzrc(caseid+15)
           endif
        endif

        normal_result(4) = PNORMAL1_4/NNORMAL2_4
        if (ieee_support_datatype(normal_result)) then
           if (ieee_is_normal(normal_result(4)) .eqv. .false. ) then
              call zzrc(caseid+16)
           endif
        endif

        normal_result(5) = PNORMAL1_4 * NNORMAL2_4
        if (ieee_support_datatype(normal_result)) then
           if (ieee_is_normal(normal_result(5)) .eqv. .false. ) then
              call zzrc(caseid+17)
           endif
        endif

!...test the range values for NANQ
!...lowest range values
        if (ieee_support_datatype(pnanq_lowest_4)) then
           if ( ieee_is_normal(pnanq_lowest_4) .eqv. .true. ) then
              call zzrc(caseid+18)
           endif
        endif

        if (ieee_support_datatype(nnanq_lowest_4)) then
           if ( ieee_is_normal(nnanq_lowest_4) .eqv. .true. ) then
              call zzrc(caseid+19)
           endif
        endif

!...highest range values
        if (ieee_support_datatype(pnanq_highest_4)) then
           if ( ieee_is_normal(pnanq_highest_4) .eqv. .true. ) then
              call zzrc(caseid+20)
           endif
        endif

        if (ieee_support_datatype(nnanq_highest_4)) then
           if ( ieee_is_normal(nnanq_highest_4) .eqv. .true. ) then
              call zzrc(caseid+21)
           endif
        endif

!...test the range values for NANS
!...lowest range values

        if (ieee_support_datatype(pnans_lowest_4)) then
           if ( ieee_is_normal(pnans_lowest_4) .eqv. .true. ) then
              call zzrc(caseid+22)
           endif
        endif

        if (ieee_support_datatype(nnans_lowest_4)) then
           if ( ieee_is_normal(nnans_lowest_4) .eqv. .true. ) then
              call zzrc(caseid+23)
           endif
        endif

!...highest range values

        if (ieee_support_datatype(pnans_highest_4)) then
           if ( ieee_is_normal(pnans_highest_4) .eqv. .true. ) then
              call zzrc(caseid+24)
           endif
        endif

        if (ieee_support_datatype(nnans_highest_4)) then
           if ( ieee_is_normal(nnans_highest_4) .eqv. .true. ) then
              call zzrc(caseid+25)
           endif
        endif

!...Check that no flags were turned on by IEEE_IS_NORMAL
        call ieee_get_flag(ieee_all, actual_flag_values)

        do k = 1,5
           if (actual_flag_values(k) .neqv. .false. ) then
              call zzrc(caseid+26)
           endif
        enddo

        end program
