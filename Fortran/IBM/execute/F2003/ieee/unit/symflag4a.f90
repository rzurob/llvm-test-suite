! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qnostrictieeemod -qfree=f90
! %GROUP: symflag4a.f
! %VERIFY: symflag4a.out:symflag4a.vf
! %STDIN:
! %STDOUT: symflag4a.out
! %EXECARGS:
! %POSTCMD: rm m2.mod
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
!*  DESCRIPTION                : test Save and Restore
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

!     _main, yyyy, xxxx have prologue/epilogue save/restore
!     the calls to xxxx are not wrapped at the callsite
!     the calls to yyyy and zzzz are wrapped at the callsite


!     _main:
!     _main has the bit set
!     The calls to xxxx, _xlfExit have the bit set
!     Everything else doesn't
      use ieee_exceptions
      interface
        subroutine xxxx()
          use ieee_exceptions
        end subroutine xxxx
      end interface

      logical :: val(5)

      call ieee_get_flag(ieee_all,val)
      print *, val

      call ieee_set_flag(ieee_overflow, .true.)

      call ieee_get_flag(ieee_all,val)
      print *, val

      call xxxx

      call ieee_get_flag(ieee_all,val)
      print *, val

      call yyyy

      call ieee_get_flag(ieee_all,val)
      print *, val

      call zzzz

      call ieee_get_flag(ieee_all,val)
      print *, val
      end

!     yyyy:
!     yyyy has the bit set.
!     Everything else doesn't
      subroutine yyyy()
        use ieee_exceptions
        logical val(5)

        call ieee_get_flag(ieee_all,val)
        print *, val

        call xxxx

        call ieee_get_flag(ieee_all,val)
        print *, val
      end subroutine yyyy

!     zzzz:
!     Nothing has the bit set
      subroutine zzzz()
        use m2
        logical val(5)
        val = get_flags()
        print *, val
        call xxxx
        val = get_flags()
        print *, val
      end subroutine zzzz

!     xxxx:
!     xxxx has the bit set.
!     The call to ieee_set_flag has the bit set
      subroutine xxxx()
         use ieee_exceptions
         call ieee_set_flag(ieee_overflow,.false.)
         call ieee_set_flag(ieee_inexact,.true.)
      end subroutine

