! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_procptr/component2/procInterface002d.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              procedure pointer is a dummy argument.
!                              The associated function is an intrinsic
!                              function.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j
    end type

    contains

    type(Child(20,4,20,4)) function func2(r, p)
        implicit real (p)
        real, intent(in) :: r
        procedure(real), pointer, intent(in) :: p
        func2 = Child(20,4,20,4)(int(p(r)),-int(p(r)))
    end function
end module

module m2
use m1
    implicit real (r), type(Child(20,4,20,4)) (p)

    type Container(k3,n3)    ! (4,20)
        integer, kind :: k3
        integer, len  :: n3
        procedure(real), pointer, nopass :: rpp1
        procedure(func2), pointer, nopass :: pp2
    end type
end module

program procInterface002d
use m2
    class(Container(4,:)), pointer :: c1
    allocate(Container(4,20)::c1)

    c1%rpp1 => sin
    c1%pp2 => func2

    print *, c1%pp2(3.1415926/2, c1%rpp1)
end
