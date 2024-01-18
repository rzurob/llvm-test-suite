! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg031a2_2.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/28/2005
!*
!*  DESCRIPTION                : argument association (dummy-arg used as the
!                               actual-arg)
!*
!*  KEYWORD(S)                 :
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

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (c)
        class (child(4,1,*)), intent(in) :: c

        print *, c%id, c%name
    end subroutine

    subroutine printBase (c)
        class (base(4)), intent(in) :: c

        print *, c%id
    end subroutine

    subroutine test1 (b)
        class (base(4)), target, intent(inout) :: b(0:)
        class (*), pointer :: b1(:)

        b1 => b(0::2)

        call test2 (b%id)

        call test2 (b1)
    end subroutine

    subroutine test2 (x)
        class (*), intent(inout) :: x(2:4)

        select type (x)
            type is (integer(4))
                print *, x
            class is (base(4))
                do i = 2, 4
                    call x(i)%print
                end do
            class default
                print *, 'unknown type'
        end select
    end subroutine
end module

program fArg031a2_1
use m
    class (base(4)), allocatable :: b1 (:)
    type (child(4,1,20)) :: c1 (10)


    allocate (b1(10), source=(/(child(4,1,20)(i,'c1_'//char(ichar('0')+i-1)), i=1,10)/))

    call test1 (b1)

    call test1 (b1(1:9:2))
end
