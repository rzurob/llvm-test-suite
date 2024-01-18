! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/component1/sequenceType003.f
! opt variations: -qnol -qreuse=self
! with manual adjustment (specify interfaceFunc1 in line 46)

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer component for
!                              sequence type. Specify proc-interface
!                              using declaration type specification.
!                              Associate the procedure pointer component
!                              with a dummy procedure pointer. The
!                              associated function is a module function.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1,k2)    ! (20,4,4)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence
        integer(k1)      i
        integer(k2)      j
    end type

    interface
        integer function interfaceFunc1(b)
        import Base
            type(Base(*,4,4)), intent(in) :: b
        end function

        type(Base(20,4,4)) function interfaceFunc2(b, p)
        import Base, interfaceFunc1
            type(Base(*,4,4)), intent(in) :: b
            procedure(interfaceFunc1), pointer, intent(in) :: p
        end function
    end interface

    type Child(n2,k3,k4)    ! (20,4,4)
        integer, kind :: k3,k4
        integer, len  :: n2
        sequence
        integer(k3)      i
        procedure(interfaceFunc1), nopass, pointer :: pp1
        integer(k4)      j
        procedure(interfaceFunc2), nopass, pointer :: pp2
    end type

    contains

    integer function func1(b)
        type(Base(*,4,4)), intent(in) :: b
        func1 = b%i + b%j
    end function

    type(Base(20,4,4)) function func2(b, p)
        type(Base(*,4,4)), intent(in) :: b
        procedure(interfaceFunc1), pointer, intent(in) :: p
        func2 = Base(20,4,4)(p(b),p(b)*2)
    end function
end module

program sequenceType003
use m
    type(Child(20,4,4)) :: b1
    b1%pp1 => func1
    b1%pp2 => func2

    associate(name1=>b1%pp2(Base(20,4,4)(10,20), b1%pp1))
        print *, "func2", name1%i, name1%j
    end associate
end
