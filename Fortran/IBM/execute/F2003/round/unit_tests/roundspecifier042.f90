!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier042.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of ROUND= specifier in WRITE statements
!*                               with specifers set at compile and runtime with
!*                               external files
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  type dt
    real :: num1
    real :: num2
    real :: num3
    real :: num4

  end type

  interface write(formatted)
    module procedure w1
  end interface

  contains



    subroutine w1(dtv, unit, iotype, v_list, iostat, iomsg)
      class(dt), intent(in) :: dtv
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(2, fmt='(f8.5,/, f8.5,/,f9.5,/, f9.5)') dtv%num1,dtv%num2, dtv%num3, dtv%num4

    end subroutine

end module

use m

  type(dt) :: dt1=dt(6.452768,6.452762,-6.452768,-6.452762  )

  open(unit=2, file='roundspecifier042.out')
  write(2, *, round='up') dt1
  write(2, *, round='down') dt1
  write(2, *, round='zero') dt1
  write(2, *, round='nearest') dt1
  write(2, *, round='compatible') dt1
  write(2, *, round='processor_defined') dt1



end
