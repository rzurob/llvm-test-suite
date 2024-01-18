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
!*  Variant of arrsec_11_1.f: with x component as var char-len component and
!*  child includes kind parameter.
!*  Also ICEs with 080921a per notes in arrsec_11_1.f
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(n1)
        integer, len :: n1
        character(n1) :: x
    end type

    type, extends(base) :: child(k,n2)
        integer, kind :: k
        integer, len :: n2
        integer(k) :: y(n2)
    end type
end module

use m

type(child(:,8,:)), allocatable :: carr(:,:)
allocate(child(3,8,4) :: carr(3,2))

carr(2:,:2)%x = reshape(['11','22','33','44'],[2,2])
carr(1:2,:)%y(1) = reshape([1,2,3,4],[2,2])
print *,carr(3:2:-1,1:2)%x
print *,carr(2:3,2:1:-1)%x
carr(2:,2:1:-1)%x = carr(3:2:-1,:)%x
carr(2:1:-1,2:1:-1)%y(1) = carr(1:2,:)%y(1)
print *,carr(2:3,2:1:-1)%x
print *,carr(1:2,:)%y(1)

end
