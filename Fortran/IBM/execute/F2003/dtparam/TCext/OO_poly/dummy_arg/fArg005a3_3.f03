! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a3_3.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (this is a variation of
!*                               test case fArg005a3_2.f; deallocate statements
!*                               are removed before calling ALLOCATE for the
!*                               same pointer to expose some of the problems
!*                               doscovered during the development)
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

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface
        subroutine copyBase (b, b1)
        import base
            class (base(4)), pointer, intent(out) :: b
            class (base(4)), intent(in) :: b1
        end subroutine

        subroutine copyBaseArray (b, b1)
        import base
            class (base(4)), pointer, intent(out) :: b(:)
            class (base(4)), intent(in) :: b1 (:)
        end subroutine
    end interface

    class (base(4)), pointer :: b1_m, b2_m(:)

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

program fArg005a3_2
use m
    class (base(4)), pointer :: b1, b2(:)
    type (child(4,1,20)) :: c1(3)
    type (base(4)) :: b3(2)

    allocate (b1, source=child(4,1,20)(1, 'b1_pointer'))

    call copyBase (b1_m, b1)

    call b1_m%print


    allocate (b1)

    b1%id = 2

    call b1%print

    call copyBase (b1_m, b1)

    call b1_m%print

    deallocate (b1, b1_m)

    c1%id = (/3,4,5/)
    c1%name = (/'c1_3', 'c1_4', 'c1_5'/)

    allocate (b2(3), source=c1)

    call copyBaseArray (b2_m, b2)

    if (size(b2_m) /= 3) error stop 1_4


    call b2_m(1)%print
    call b2_m(2)%print
    call b2_m(3)%print


    b3%id = (/6, 7/)

    allocate (b2(2), source=b3)

    call copyBaseArray (b2_m, b2)

    if (size(b2_m) /= 2) error stop 2_4


    call b2_m(1)%print
    call b2_m(2)%print

    deallocate (b2, b2_m)
end


subroutine copyBase (b, b1)
use m, only : base
    class (base(4)), pointer, intent(out) :: b
    class (base(4)), intent(in) :: b1

    allocate (b, source=b1)
end subroutine


subroutine copyBaseArray (b, b1)
use m, only : base
    class (base(4)), pointer, intent(out) :: b (:)
    class (base(4)), intent(in) :: b1 (:)

    allocate (b(size(b1)), source=b1)
end subroutine