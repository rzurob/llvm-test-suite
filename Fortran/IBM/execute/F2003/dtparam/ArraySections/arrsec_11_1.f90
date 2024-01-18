!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_1.f
!*  DATE                       : Sep 23, 2008
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  6.2.2.3 Array sections
!*  Variant of arrsec_11.f: with x component an array.
!*
!*  This caused an ICE in ASTI with current (080921a) DFS driver, but seems to
!*  pass with dixon's xlfentry.
!*  0x000007c0 in wilcu::create_adr(WN*,WN*,long)
!*  0x00000124 in wilcu::flatten_address(WN*,WN*)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(n1)
        integer, len :: n1
         integer :: x(n1)
    end type

    type, extends(base) :: child(n2)
        integer, len :: n2
        integer :: y(n2)
    end type
end module

use m

type(child(:,:)), allocatable :: carr(:,:)
allocate(child(3,4) :: carr(3,2))

carr(2:,:2)%x(1) = reshape([1,2,3,4],[2,2])
carr(1:2,:)%y(1) = reshape([1,2,3,4],[2,2])
print *,carr(3:2:-1,1:2)%x(1)
print *,carr(2:3,2:1:-1)%x(1)
carr(2:,2:1:-1)%x(1) = carr(3:2:-1,:)%x(1)
carr(2:1:-1,2:1:-1)%y(1) = carr(1:2,:)%y(1)
print *,carr(2:3,2:1:-1)%x(1)
print *,carr(1:2,:)%y(1)

end
