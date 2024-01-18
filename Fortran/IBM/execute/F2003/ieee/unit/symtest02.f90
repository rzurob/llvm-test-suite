! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qnostrictieeemod -qfree=f90
! %GROUP: symtest02.f
! %VERIFY: symtest02.out:symtest02.vf
! %STDIN:
! %STDOUT: symtest02.out
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
!*  PROGRAMMER                 : Rafik Zurob
!*  DATE                       : March, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Save and Restore 
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : test save and restore for procedures
!*                               that use ieee
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

!     xx uses ieee modules, so it should have prologue/epilogue save and restore
!     xx contains an explicit interface to yy saying that yy uses ieee modules,
!       so there should be no callsite save and restore around yy.
!     zz is external to xx, so xx will save and restore around the callsite
!     No save and restore should be done around I/O routines
      subroutine xx()
         use ieee_arithmetic
         
         interface
           subroutine yy()
             use ieee_exceptions
           end subroutine yy
         end interface
         
         logical :: val(5),expected(5)
         integer :: i
         type(ieee_round_type) :: rndmode
         
         call ieee_set_flag(ieee_divide_by_zero, .true.)
         ! div_by_zero should be set in xx
         call yy()
         call zz()
         call ieee_get_flag(ieee_all, val)
         !              OV       DBZ       INV     UND      INEX
         expected = (/ .true. , .true. , .false., .true., .false. /)
         do i=1,5
           if (val(i) .neqv. expected(i)) then
             print *, "Fail - val(",i,") is not ", expected(i)
           endif
         enddo
         call ieee_get_rounding_mode(rndmode)
         if (rndmode /= ieee_nearest) then
           print *, "Fail - rounding mode changed"
         endif
      end subroutine xx

      subroutine yy()
         use ieee_exceptions
         logical :: val
         call ieee_get_flag(ieee_divide_by_zero, val)
         if (val .eqv. .true.) then
           print *, "Fail - div_by_zero should be clear"
         else
           print *, "OK - div_by_zero is clear in yy"
         endif
         call ieee_set_flag(ieee_divide_by_zero, .false.)
         call ieee_set_flag(ieee_overflow, .true.)
      end subroutine

      subroutine zz()
        use xlf_fp_util
        integer(fpscr_kind) :: oldrnd
        ! does NOT use ieee_exceptions
        call clr_fpscr_flags(fp_overflow)
        call set_fpscr_flags(fp_underflow)
        oldrnd = set_round_mode(fp_rnd_rp)
      end subroutine

      

      
      use ieee_exceptions
      
      interface 
        subroutine xx()
        use ieee_exceptions
        end
      end interface
      
      print *, "The FE set the IEEE_FPSCR bit on xx, yy, and _main"
      print *, "zz does not have the bit set."
      print *, "The IO library functions also have the bit set."
      print *, " "
      print *, "              1       2      3         4         5"
      print *, "val() = (/ overflow, dbz, invalid, underflow, inexact"
      print *, " "
      print *, "If there are errors, they will appear below:"
      call xx
      end
