! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/others/dataPtrDefineAssgn2.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!*  - defined assignment a1 = a2 where a1 & a2 are ptr of type DT of diff rank
!*  - subourtine for = is module procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(n1,k1)    ! (20,4)
        integer, kind        :: k1
        integer, len         :: n1
        integer(k1), pointer :: id(:)
    end type

    contains

    subroutine assgnPtr(p1, p2)
         type(base(:,4)), pointer, intent(inout) :: p1(:,:)
         class(base(*,4)),pointer,  intent(in) :: p2(:)

         p1(1:10,-10:-1) => p2
    end subroutine
end module

program main
use m

    type(base(:,4)), pointer :: p1(:,:)
    class(base(20,4)), pointer :: p2(:)
    type(base(20,4)), target :: tar1(400)
    integer, target, allocatable ::  t(:,:)

    allocate(t(400,2))

    do i = 1, 400
        t(i,:) =(/ i, i+ 1 /)
    enddo

    tar1 =  (/(base(20,4)( t(i,:) ),i=1,400)/)

    i = 51

    p2(tar1(i)%id(1):tar1(199)%id(2)) => tar1(i:)

!    p1 = p2
    call assgnPtr (p1, p2)

    if ( .not. associated(p1)) error stop 11

    if (any(lbound(p1) .ne. (/1, -10/))) error stop 21
    if (any(ubound(p1) .ne. (/10, -1/))) error stop 23

    print *, (/ ((p1(i,j)%id, i=lbound(p1,1),ubound(p1,1)), &
         j= lbound(p1,2),ubound(p1,2)) /)

End program

