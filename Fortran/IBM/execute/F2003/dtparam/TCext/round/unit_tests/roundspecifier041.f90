! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/round/unit_tests/roundspecifier041.f
! opt variations: -qnol -qreuse=none

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
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
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    real(k1)      :: num1
    real(k1)      :: num2
    real(k1)      :: num3
    real(k1)      :: num4

  end type

  interface read(formatted)
    module procedure r1
  end interface

  contains



    subroutine r1(dtv, unit, iotype, v_list, iostat, iomsg)
      class(dt(*,4)), intent(inout) :: dtv
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      read(2, fmt='(f8.6,/, f8.6,/,f9.6,/, f9.6)') dtv%num1,dtv%num2, dtv%num3, dtv%num4

    end subroutine

end module

use m

  type(dt(20,4)) :: dt1

  open(unit=2, file='real4.dat')

  read(2, *, round='up') dt1

  if (dt1%num1 .ne. z'40CE7D14') error stop 1
  if (dt1%num2 .ne. z'40CE7D07') error stop 2
  if (dt1%num3 .ne. z'C0CE7D13') error stop 3
  if (dt1%num4 .ne. z'C0CE7D06') error stop 4

  rewind 2

  read(2, *, round='down') dt1

  if (dt1%num1 .ne. z'40CE7D13') error stop 5
  if (dt1%num2 .ne. z'40CE7D06') error stop 6
  if (dt1%num3 .ne. z'C0CE7D14') error stop 7
  if (dt1%num4 .ne. z'C0CE7D07') error stop 8

  rewind 2

  read(2, *, round='zero') dt1

  if (dt1%num1 .ne. z'40CE7D13') error stop 9
  if (dt1%num2 .ne. z'40CE7D06') error stop 10
  if (dt1%num3 .ne. z'C0CE7D13') error stop 11
  if (dt1%num4 .ne. z'C0CE7D06') error stop 12

  rewind 2

  read(2, *, round='nearest') dt1

  if (dt1%num1 .ne. z'40CE7D13') error stop 13
  if (dt1%num2 .ne. z'40CE7D07') error stop 14
  if (dt1%num3 .ne. z'C0CE7D13') error stop 15
  if (dt1%num4 .ne. z'C0CE7D07') error stop 16

  rewind 2

  read(2, *, round='compatible') dt1

  if (dt1%num1 .ne. z'40CE7D13') error stop 17
  if (dt1%num2 .ne. z'40CE7D07') error stop 18
  if (dt1%num3 .ne. z'C0CE7D13') error stop 19
  if (dt1%num4 .ne. z'C0CE7D07') error stop 20

  rewind 2

  read(2, *, round='processor_defined') dt1

  if (dt1%num1 .ne. z'40CE7D13') error stop 21
  if (dt1%num2 .ne. z'40CE7D07') error stop 22
  if (dt1%num3 .ne. z'C0CE7D13') error stop 23
  if (dt1%num4 .ne. z'C0CE7D07') error stop 24



end
