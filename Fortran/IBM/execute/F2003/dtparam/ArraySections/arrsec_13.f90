!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sep 26, 2008
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type plist(n)
        integer, len :: n
        integer :: arr(n)
        procedure(pinterface), pointer, pass(this) :: proc
    end type

    interface
        subroutine pinterface ( argarr, this )
             import
            integer :: argarr(:)
            class(plist(*)) :: this
        end subroutine
    end interface

    contains
        subroutine sub1 ( argarr, this )
            integer :: argarr(:)
            class(plist(*)) :: this
            call sub2(argarr,this%arr)
        end subroutine

        subroutine sub2 ( argarr, thisarr )
            integer :: argarr(:)
            integer :: thisarr(:)
            print *,'in sub2 dummy-arr=',argarr
            print *,'in sub2 this->arr=',thisarr
        end subroutine
end module

use m
integer, parameter :: L=4, N=5
type(plist(:)), allocatable :: plistarr(:)
allocate(plist(L) :: plistarr(N))

! as of 080925 aix 13.1 driver, the => below causes garbage printed due to
! offset problem in defect 332945
!
do i=1,N
    plistarr(i)%arr = [(i*j,j=1,L)]
    plistarr(i)%proc => sub1
end do

do i=1,N
    call plistarr(i)%proc(plistarr(i:N)%arr(L))
end do
end
