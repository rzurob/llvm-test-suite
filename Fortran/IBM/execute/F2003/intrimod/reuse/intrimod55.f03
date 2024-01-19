! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : January, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : -qnostrictieeemod
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Re-using /tstdev/ieee/unit/symflag8a.f
!*                               to test INTRINSIC module nature.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

!     xxxx, uuuu should have save-restore in pro-epilogue

!     zzzz, and _main should not have any save / restores of any kind.

!     There should be no save-restore around _xlfBeginIO, _xlfEndIO, ... etc

!     The call to yyyy should not have any callsite save / restore because yyyy in
!     an entry to a procedure that uses ieee


!     m:
!     hidden module procedure does not have the  bit set
      module m
      contains
!       xxxx:
!       xxxx, yyyy, vvvv have the bit set
!       The calls to _xlfBeginIO, _xlfWriteLDLog, _xlfEndIO have the bit set
        subroutine xxxx()
          use, intrinsic :: ieee_exceptions
          logical :: val(5)

          call ieee_get_flag(ieee_all,val)
          print *, val
          call ieee_set_flag(ieee_overflow, .true.)

          entry yyyy()

          call ieee_get_flag(ieee_all,val)
          print *, val
          call ieee_set_flag(ieee_underflow, .true.)

          entry vvvv()

          call ieee_get_flag(ieee_all,val)
          print *, val
          call ieee_set_flag(ieee_divide_by_zero, .true.)

          call ieee_set_flag(ieee_inexact, .false.)
          call ieee_get_flag(ieee_all,val)
          print *, val
        end subroutine xxxx
      end module m

      module m2
        use, intrinsic :: xlf_fp_util
      contains
        ! print the status of all exception flags
        function get_flags()
          use, intrinsic :: xlf_fp_util
          logical, dimension(5) :: get_flags
          integer(fpscr_kind), dimension(5) :: flags
          integer i

          flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)
          do i=1,5
            get_flags(i) = (get_fpscr_flags(flags(i)) /= 0)
          end do
        end function
      end module m2


!     zzzz:
!     zzzz does not have the bit set
!     The call to xxxx has the bit set
      subroutine zzzz()
        use m
        use m2
        logical :: val(5)
        val = get_flags()
        print *, val

        call clr_fpscr_flags(fp_overflow)
        call set_fpscr_flags(fp_inexact)

        val = get_flags()
        print *, val

        call xxxx

        val = get_flags()
        print *, val
      end subroutine zzzz

!     uuuu:
!     uuuu has the bit set
!     The call to yyyy has the bit set
      subroutine uuuu()
        use, intrinsic :: ieee_exceptions
        use m
        logical :: val(5)
        call ieee_get_flag(ieee_all, val)
        print *, val

        call ieee_set_flag(ieee_overflow, .false.)
        call ieee_set_flag(ieee_inexact, .true.)

        call ieee_get_flag(ieee_all, val)
        print *, val

        call yyyy

        call ieee_get_flag(ieee_all, val)
        print *, val
      end subroutine

!     _main:
!     The call to _xlfExit has the bit set
!     Everything else doesn't
      call zzzz
      call uuuu
      end
