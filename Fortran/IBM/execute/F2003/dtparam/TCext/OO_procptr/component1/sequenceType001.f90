! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qreuse=self /tstdev/OO_procptr/component1/sequenceType001.f
! opt variations: -qnol -qdefaultpv -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer component for
!                              sequence type. Procedure interface is
!                              specified using interface-name.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        sequence
        integer(k1)      i
        integer(k1)      j
    end type

    interface
        subroutine sub1(i, b)
        import Base
            integer, intent(in) :: i
            type(Base(*,4)), intent(in) :: b
        end subroutine

        function func1(i, j)
        import Base
            integer, intent(in) :: i
            integer, intent(in) :: j
            type(Base(20,4)) :: func1
        end function
    end interface

    type Seq(n2,k2)    ! (20,4)
        integer, kind     :: k2
        integer, len      :: n2
        sequence
        integer(k2)          k
        procedure(sub1), nopass, pointer :: pp1
        type(Base(n2,k2)) :: b
        procedure(func1), nopass, pointer :: pp2
    end type
end module

program sequenceType001
use m
    type(Seq(20,4)) :: s1
    s1%k = 10
    s1%b = Base(20,4)(20, 30)
    s1%pp1 => sub1
    s1%pp2 => func1

    call s1%pp1(11, s1%b)
    print *, "func1", s1%pp2(s1%k, 12)
end

subroutine sub1(i, b)
use m, only : Base
    integer, intent(in) :: i
    type(Base(*,4)), intent(in) :: b
    print *, "sub1", i, b
end subroutine

function func1(i, j)
use m, only : Base
    integer, intent(in) :: i
    integer, intent(in) :: j
    type(Base(20,4)) :: func1
    func1 = Base(20,4)(i, j)
end function
