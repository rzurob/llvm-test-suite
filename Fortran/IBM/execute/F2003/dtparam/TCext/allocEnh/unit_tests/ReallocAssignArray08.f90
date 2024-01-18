! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArray08.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with components of derived types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type dt(k1)    ! (4)
  integer, kind            :: k1
  integer(k1), allocatable :: i(:)
end type
type dt2(k2)    ! (4)
  integer, kind :: k2
  integer(k2)   :: i(2)
end type
type(dt(4)) :: x
type(dt2(4)) :: y
x%i = y%i
if (.not. allocated(x%i)) stop 1
if (any(shape(x%i) .ne. (/2/))) stop 2
end
