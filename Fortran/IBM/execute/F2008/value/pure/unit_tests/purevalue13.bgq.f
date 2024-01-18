!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue13.bgq.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - pass vector data type as dummy arg with value attributes to pure
!*    procedures
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

implicit none

vector(real(8)) :: vr8
real(8) :: sr8(4) = [81.82,83.84,85.86,87.88]
equivalence(vr8,sr8)

call sub(vr8,1,2)
if (any(sr8 /= [81.82,83.84,85.86,87.88])) stop 1

if (func(10,vr8,11) /= 21) stop 2
if (any(sr8 /= [81.82,83.84,85.86,87.88])) stop 3

contains
    pure subroutine sub (vr8,x,y)
        vector(real(8)), value :: vr8
        integer, value :: x,y
        real(8) :: r8(4)
        integer :: i,tmp
        r8 = [(vec_extract(vr8,i),i=0,3)]
        if (any(r8 .ne. [81.82,83.84,85.86,87.88])) tmp = tmp/0
        if (x /= 1 .or. y /= 2) tmp = tmp/0
        vr8 = transfer([0,0,0,0], vr8)
        x = 0
        y = 0
    end subroutine

    integer pure function func (x,vr8,y)
        vector(real(8)), value :: vr8
        integer, value :: x,y
        real*8 :: r8(4)
        integer :: i,tmp
        r8 = [(vec_extract(vr8,i),i=0,3)]
        if (any(r8 .ne. [81.82,83.84,85.86,87.88])) tmp = tmp/0
        if (x /= 10 .or. y /= 11) tmp = tmp/0
        func = x+y
        vr8 = transfer([0,0,0,0], vr8)
        x = 0
        y = 0
    end function
end
