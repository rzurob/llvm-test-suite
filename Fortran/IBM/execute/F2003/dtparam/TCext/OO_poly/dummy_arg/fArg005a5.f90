! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/dummy_arg/fArg005a5.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a5.f
! %VERIFY: fArg005a5.out:fArg005a5.vf
! %STDIN:
! %STDOUT: fArg005a5.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (non-poly dummy args'
!*                               association)
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
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg005a5
use m
    type (base(4)), pointer :: b1, b2 (:)
    type (child(4,1,20)), pointer :: c1, c2 (:)

    allocate (b1, source = base(4)(10))
    allocate (c1, source = child(4,1,20) (20, 'c1_pointer'))

    call abc (b1, c1)

    call b1%print
    call c1%print

    deallocate (b1, c1)

    b2 => null()
    nullify (c2)

    call cba (b2, c2)

    if ((size (b2) /= 1) .or. (size (c2) /= 1)) error stop 1_4

    call b2(1)%print
    call c2(1)%print

    deallocate (b2, c2)

    allocate (b2 (2), source=base(4)(15))
    allocate (c2(3), source=child(4,1,20)(25, 'c2_array_of_3'))

    call cba (b2, c2)

    if ((size (b2) /= 2) .or. (size (c2) /= 3)) error stop 2_4

    call b2(1)%print
    call b2(2)%print

    call c2(1)%print
    call c2(2)%print
    call c2(3)%print

    deallocate (b2, c2)


    contains

    subroutine abc (b, c)
        type (base(4)), pointer :: b
        type (child(4,1,*)), pointer :: c

        if (associated (b)) deallocate (b)

        if (associated (c)) deallocate (c)

        allocate (b, c)
    end subroutine

    subroutine cba (b, c)
        type (base(4)), pointer :: b (:)
        type (child(4,1,*)), pointer :: c(:)

        integer*4 :: b_size, c_size

        b_size = 1
        c_size = 1

        if (associated (b)) then
            b_size = size(b)
            deallocate (b)
        end if

        if (associated (c)) then
            c_size = size(c)
            deallocate (c)
        end if

        allocate (b(b_size), c(c_size))
    end subroutine
end
