! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_procptr/declaration2/procInterface002d.f
! opt variations: -ql -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
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

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    type(Child(4)) function func2(r, p)
        implicit real (p)
        real, intent(in) :: r
        procedure(), pointer, intent(in) :: p
        func2 = Child(4)(int(p(r)),-int(p(r)))
    end function
end module

program procInterface002d
use m
    implicit real (r), type(Child(4)) (p)

    procedure(), pointer :: rpp1
    procedure(func2), pointer :: pp2

    rpp1 => sin
    pp2 => func2

    print *, pp2(3.1415926/2, rpp1)
end
