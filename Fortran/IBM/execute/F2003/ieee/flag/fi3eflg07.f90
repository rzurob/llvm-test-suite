! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:  -qfree=f90
! %GROUP: fi3eflg07.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : GET_FPSCR_FLAGS, SET_FPSCR_FLAGS
!*  SECONDARY FUNCTIONS TESTED : CLR_FPSCR_FLAGS
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SUBROUTINE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing get_fpscr_flags(),set_fpscr_flags()
!*                               and clr_fpscr_flags() subroutines.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fi3eflg07

        use xlf_fp_util

        integer(fpscr_kind) :: flg_val(5)

!check if all flags are cleared
        flg_val(1) = get_fpscr_flags(fp_overflow)
        flg_val(2) = get_fpscr_flags(fp_div_by_zero)
        flg_val(3) = get_fpscr_flags(fp_invalid)
        flg_val(4) = get_fpscr_flags(fp_underflow)
        flg_val(5) = get_fpscr_flags(fp_inexact)
        if (any(flg_val .ne. 0))  then
           error stop 1
        endif

!set all flags to true
!Force ieee_invalid to TRUE
        call set_fpscr_flags(fp_inv_isi)

        call set_fpscr_flags(fp_overflow)
        call set_fpscr_flags(fp_div_by_zero)
        call set_fpscr_flags(fp_underflow)
        call set_fpscr_flags(fp_inexact)

!check if they are true
        flg_val(1) = get_fpscr_flags(fp_overflow)
        flg_val(2) = get_fpscr_flags(fp_div_by_zero)
        flg_val(3) = get_fpscr_flags(fp_invalid)
        flg_val(4) = get_fpscr_flags(fp_underflow)
        flg_val(5) = get_fpscr_flags(fp_inexact)

        if (any(flg_val .eq. 0 )) error stop 2

!set back all flags to false by using clr_fpscr_flags()
        call clr_fpscr_flags(fp_overflow)
        call clr_fpscr_flags(fp_div_by_zero)
        call clr_fpscr_flags(fp_underflow)
        call clr_fpscr_flags(fp_inexact)

!Clear all invalid possibilities:
        call clr_fpscr_flags(fp_inv_isi)
        call clr_fpscr_flags(fp_inv_idi)
        call clr_fpscr_flags(fp_inv_zdz)
        call clr_fpscr_flags(fp_inv_cmp)
        call clr_fpscr_flags(fp_inv_sqrt)
        call clr_fpscr_flags(fp_inv_cvi)
        call clr_fpscr_flags(fp_inv_vxsoft)

!...check if they are false using xlf_fp_util procedures
        flg_val(1) = get_fpscr_flags(fp_overflow)
        flg_val(2) = get_fpscr_flags(fp_div_by_zero)
        flg_val(3) = get_fpscr_flags(fp_invalid)
        flg_val(4) = get_fpscr_flags(fp_underflow)
        flg_val(5) = get_fpscr_flags(fp_inexact)


        if (any(flg_val .ne. 0))  then
           error stop 4
        endif

        end program

