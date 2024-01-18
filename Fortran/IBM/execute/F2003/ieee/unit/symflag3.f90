! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: symflag3.f
! %VERIFY: symflag3.out:symflag3.vf
! %STDIN:
! %STDOUT: symflag3.out
! %EXECARGS:
! %POSTCMD: rm m2.mod m.mod
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : Save and Restore
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : test save and restore
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module m2
        use xlf_fp_util
      contains
        ! print the status of all exception flags
        function get_flags()
          use xlf_fp_util
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

!     yyyy should have prologue/epilogue save/restore
!     xxxx should *not* have prologue/epilogue save/restore
!     the call to xxxx should be wrapped at the callsite


!     m:
!     hidden module procedure doesn't have the bit set.
      module m
        use ieee_exceptions
      contains
!       xxxx:
!       The calls to _xlfBeginIO, _xlfWriteLDChar, _xlfEndIO have the bit set
!       everything else doesn't
        subroutine xxxx()
          use m2
          logical val(5)
          val = get_flags()
          print *, val

          call set_fpscr_flags(fp_overflow)
          call clr_fpscr_flags(fp_inexact)

          val = get_flags()
          print *, val
          print *, "sub xxxx does not use ieee_exceptions"
        end subroutine xxxx
      end module m


!     yyyy:
!     yyyy has the bit set
!     The calls to_xlfBeginIO, _xlfWriteLDChar, _xlfEndIO have the bit set
!     everything else doesn't
      subroutine yyyy()
        use m
        logical :: val(5)
        call ieee_get_flag(ieee_all,val)
        print *, val

        call ieee_set_flag(ieee_underflow, .false.)
        call ieee_get_flag(ieee_all,val)
        print *, val

        call xxxx

        call ieee_get_flag(ieee_all,val)
        print *, val
        print *, "sub yyyy uses ieee_exceptions"
      end subroutine yyyy

!     _main:
!     The call to _xlfExit has the bit set
!     Everything else doesn't have the bit  (y is external so can't tell that
!     y uses ieee.)
      use m2
      logical val(5)
      val = get_flags()
      print *, val

      call set_fpscr_flags(fp_underflow)
      call set_fpscr_flags(fp_inexact)
      val = get_flags()
      print *, val

      call yyyy

      val = get_flags()
      print *, val
      end
