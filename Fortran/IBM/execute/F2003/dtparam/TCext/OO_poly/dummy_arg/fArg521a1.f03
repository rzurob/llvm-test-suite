! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg521a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/01/2005
!*
!*  DESCRIPTION                : argument association (poly-entities in array
!                               constructors that is used as the actual-arg)
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
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    elemental subroutine assignID (b, idStart)
        class (base(4)), intent(inout) :: b
        integer, intent(in) :: idStart

        b%id = idStart
    end subroutine

    elemental subroutine assignName (b, name)
        class (base(4)), intent(inout) :: b
        character(*), intent(in) :: name

        select type (b)
            type is (child(4,1,*))
                b%name = name
            class default
                return
        end select
    end subroutine

    subroutine printVal (b)
        class (base(4)), intent(in) :: b(:)

        do i = 1, size(b)
            select type (x => b(i))
                type is (base(4))
                    print *, x%id
                type is (child(4,1,*))
                    print *, x%id, x%name
                class default
                    error stop 1_4
            end select
        end do
    end subroutine
end module


program fArg521a1
use m
    class (base(4)), allocatable :: b1(:,:)
    class (base(4)), pointer :: b2(:,:)

    allocate (child(4,1,20) :: b1(2,2))
    allocate (child(4,1,20) :: b2(3,1))

    !! set the values for b1 and b2
    call assignID (b1, reshape ((/1,2,3,4/), (/2,2/)))
    call assignID (b2, reshape ((/10, 20, 30/), (/3,1/)))

    call assignName (b1, reshape ((/'b1 1', 'b1 2', 'b1 3', 'b1 4'/), (/2,2/)))
    call assignName (b2, reshape ((/'b2 1', 'b2 2', 'b2 3'/), (/3,1/)))


    !! print values
    print *, 'test 1'

    call printVal ((/b1, b2/))

    print *, new_line('a'), 'test 2'
    !! This line may be hard to read: array sections with vector subscript in
    !array section
    call printVal ((/b1((/2,1/), (/1/)), b2 ((/3,1/), (/1,1/))/))


    !! test a zero-size array
    print *, new_line('a'), 'test 3'

    call printVal ((/b1(1:0,:), b2(2:1,:)/))
end
