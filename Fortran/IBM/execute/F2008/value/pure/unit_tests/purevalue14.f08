!******************************************************************************
!*  ===========================================================================
!*
!*  DATE            : 2010-12-01
!*
!*  DESCRIPTION
!*  - this is a copy of purevalue03.f, modified to define the pure procedures
!*    externally, and declare them here through explicit interface for func1,
!*    sub1, and through module interface for func2 and sub2, and through use
!*    association for func3, sub3
!234567890123456789012345678901234567890123456789012345678901234567890123456789

use m
implicit none

interface
    pure integer function func1 (a)
        integer, value :: a
    end function

    pure subroutine sub1 (a)
        integer, value :: a
    end subroutine
end interface

integer :: sa=1, sb=2, sc, sr

sc = func1(sa)
if (sc /= sa) error stop 1
if (sa /= 1) error stop 2

sc = func2(sa,sb)
if (sc /= (sa+sb)) error stop 3
if (sb /= 2) error stop 4

sr = func3(sa,sb,sc,sa,sb,sc)
if (sr /= 12) error stop 5
if (any([sa,sb,sc] /= [1,2,3])) error stop 6

call sub1(sa)
if (sa /= 1) error stop 7

call sub2(sa,sb)
if (any([sa,sb] /= [1,2])) error stop 8

call sub3(sa,sb,sc,sa,sb,sc)
if (any([sa,sb,sc] /= [-10,2,1])) error stop 8

end