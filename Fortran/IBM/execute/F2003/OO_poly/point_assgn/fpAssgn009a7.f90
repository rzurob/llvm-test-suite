!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2005
!*
!*  DESCRIPTION                : data pointer assignment (sequence type pointer
!                               assigned to unlimited poly target in the select
!                               type construct under class default type-guard
!                               statement)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

!! this subroutine only takes actual-arg of these dynamic types: integer,
!character and sequence type seq1
subroutine printArrayX (x)
    class (*), target, intent(in) :: x(:)

    type seq1
        sequence
        integer(4) i
        integer(8) j
    end type

    type (seq1), pointer :: s1(:)

    select type (x)
        type is (integer)
            print *, x
        type is (character(*))
            print *, x
        class default
            print *, 'Warning: assume a sequence type input'
            s1 => x

            print *, s1
    end select
end subroutine

program fpAssgn009a7
    interface
        subroutine printArrayX (x)
            class (*), target, intent(in) :: x(:)
        end subroutine
    end interface

    type seq1
        sequence
        integer(4) i
        integer(8) j
    end type

    character(20), pointer :: c1(:)
    type (seq1), target :: s2(2,2)

    allocate (c1(3))

    c1 = (/'ibm', 'xlf', 'com'/)

    s2 (:,1) = (/seq1(1,10), seq1(2,20)/)
    s2 (:,2) = (/seq1(-1,-10), seq1(-2,-20)/)

    call printArrayX (c1)
    call printArrayX ((/10,9,8,7,6,5,4,3,2,1/))
    call printArrayX ((/s2/))
end
