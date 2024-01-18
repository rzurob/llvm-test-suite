! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass003.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass003.f
! %VERIFY: fclass003.out:fclass003.vf
! %STDIN:
! %STDOUT: fclass003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (final binding with defined
!                               assignment)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: data(:) => null()

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    subroutine finalizeBase(b)
        type (base(4)), intent(inout) :: b

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        else
            print *, 'data is not associated'
        end if
    end subroutine
end module

program fclass003
use m
    interface assignment (=)
        subroutine baseAssgn2Base (a, b)
        use m
            type (base(4)), intent(out) :: a
            type (base(4)), intent(in) :: b

        end subroutine
    end interface

    type (base(4)) b1, b2, b3
    type (child(4,1,20)) c1

    allocate (b1%data(10))

    b1%data = (/(i, i=1, 10)/)

    b2 = b1

    if (associated (b2%data, b1%data)) error stop 1_4

    print *, b2%data

    b2 = b3

    print *, 'after'

    if (associated (b2%data))  error stop 2_4

    c1%base = b1

    if (associated (c1%data, b1%data)) error stop 3_4

    print *, c1%data

    c1%base = b2

    if (associated (c1%data)) error stop 4_4
end

subroutine baseAssgn2Base (a, b)
use m
    type (base(4)), intent(out) :: a
    type (base(4)), intent(in) :: b

    if (associated (a%data)) error stop 1_4

    if (associated (b%data)) then
        allocate (a%data(size(b%data)))

        a%data = b%data
    end if
end subroutine
