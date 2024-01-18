!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue16.f
!*  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
!*  PROGRAMMER      : Gaby Baghdadi
!*  DATE            : 2010-12-01
!*  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - dummy data objects with value attribute before and after extended derived
!*    type dummy argument
!*  - the derived type has explicit as well as allocatable arrays that are 
!*    modified in the pure procedures
!*  - see defect 385516 on array limitation (hence the small ranks size used)
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789
module m
    implicit none

    type base
        integer :: i
        character*3 :: c
    end type
    
    type, extends(base) :: child
        character(:), allocatable :: cc
        integer :: ii
        integer, allocatable :: aiarr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer :: jj
    end type
end module
    
use m
implicit none
type(child) :: co = child(12,'abc',null(),34,null(),56)
integer :: i,x,a=1,b=2
integer :: aiarr(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2)

co%cc = 'xyz'
allocate(co%aiarr(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2))
co%aiarr = reshape([(i,i=1,2*3*2*3*2*3*2*3*2*3*2*3*2)],[1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2])
aiarr = reshape([(i,i=1,2*3*2*3*2*3*2*3*2*3*2*3*2)],[1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2])

x = foo(a,co,b)
call checkres
call soo(x,a,co,b)
call checkres
x = efoo(a,co,b)
call checkres
call esoo(x,a,co,b)
call checkres

contains
    integer pure function foo(va,vb,vc)
        integer, value :: va
        type(child), value :: vb
        integer :: vc
        value :: vc

        vb%i = x'12345678'
        vb%c = 'xyz'
        vb%ii = x'90123456'
        vb%jj = x'78901234'
        vb%cc = 'abc'
        if (any(vb%aiarr /= aiarr)) then
            foo = -1
        else
            vb%aiarr = vb%aiarr * 2
            foo = va+vc
            va = 88
            vc = 99 
        endif
    end function

    integer elemental function efoo(va,vb,vc)
        integer, value :: va
        integer :: vc
        value :: vc
        type(child), value :: vb
        vb%i = x'12345678'
        vb%c = 'xyz'
        vb%ii = x'90123456'
        vb%jj = x'78901234'
        vb%cc = 'abc'
        if (any(vb%aiarr /= aiarr)) then
            efoo = -1
        else
            vb%aiarr = vb%aiarr * 2
            efoo = va+vc
            va = 88
            vc = 99 
        endif
    end function

    pure subroutine soo(res,va,vb,vc)
        integer, intent(out) :: res
        integer, value :: va
        integer :: vc
        value :: vc
        type(child), value :: vb
        vb%i = x'12345678'
        vb%c = 'xyz'
        vb%ii = x'90123456'
        vb%jj = x'78901234'
        vb%cc = 'abc'
        if (any(vb%aiarr /= aiarr)) then
            res = -1
        else
            vb%aiarr = vb%aiarr * 2
            res = va+vc
            va = 88
            vc = 99 
        endif
    end subroutine

    elemental subroutine esoo(res,va,vb,vc)
        integer, intent(out) :: res
        integer, value :: va
        integer :: vc
        value :: vc
        type(child), value :: vb
        vb%i = x'12345678'
        vb%c = 'xyz'
        vb%ii = x'90123456'
        vb%jj = x'78901234'
        vb%cc = 'abc'
        if (any(vb%aiarr /= aiarr)) then
            res = -1
        else
            vb%aiarr = vb%aiarr * 2
            res = va+vc
            va = 88
            vc = 99 
        endif
    end subroutine

    subroutine checkres
        if (a /= 1) stop 1
        if (b /= 2) stop 2
        if (x /= (a+b)) stop 3
        if (co%i /= 12) stop 4
        if (co%c /= 'abc') stop 5
        if (co%ii /= 34) stop 6
        if (co%jj /= 56) stop 7
        if (co%cc /= 'xyz') stop 8
        if (co%c /= 'abc') stop 9
        if (any(co%aiarr /= aiarr)) stop 10
    end subroutine
end
