! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/dummy_arg/fArg010a5.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg010a5.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute; non-poly
!                               dummy-arg associated with various actual arg)
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
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    type (child(4,1,20)), protected :: c1 = child(4,1,20) (20, 'c1')
end module

program fArg010a5
use m
    class (base(4)), pointer :: b1
    type (child(4,1,20)), parameter :: c_const = child(4,1,20) (10, 'c_const')

    class (child(4,1,20)), allocatable :: c2 (:)

    allocate (b1, source=child(4,1,20)(1, 'b1_pointer'))

    allocate (c2(3:5))


    call abc (b1, b1%id)

    if (b1%id /= 1) error stop 1_4

    call abc (c_const%base, c_const%id)

    if (c_const%id /= 10) error stop 2_4

    call abc (c1%base, c1%id)

    if (c1%id /= 20) error stop 3_4

    c2%id = (/30, 40, 50/)
    c2%name = 'c2'

    do i = 3, 5
        call abc (c2(i)%base, i*10)
    end do

    deallocate (b1)

    contains

    subroutine abc (b, i)
        type (base(4)), value :: b
        integer*4, value :: i

        if (b%id /= i) error stop 100_4

        b%id = b%id + 100
        i = b%id
    end subroutine
end
