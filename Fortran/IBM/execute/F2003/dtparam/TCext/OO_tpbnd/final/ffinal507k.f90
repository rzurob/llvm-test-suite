! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal507k
!*
!*  DATE                       : 2007-11-01 (original: 02/10/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of the zero-size array)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), pointer :: id => null()

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1), pointer :: name => null()

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    interface assignment(=)
        module procedure b1Assgnb2
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

        if (associated (b%id)) then
            print *, 'deallocate id'

            deallocate (b%id)
        end if
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(inout) :: c ! tcx: (4,*)

        print *, 'finalizeChild'

        if (associated (c%name)) then
            print *, 'deallocate name'

            deallocate (c%name)
        end if
    end subroutine

    subroutine finalizeChildArray1 (c)
        type (child(4,*)), intent(inout) :: c(:) ! tcx: (4,*)

        print *, 'finalizeChildArray1'

        do i = 1, size (c)
            call finalizeChild (c(i))
        end do
    end subroutine

    subroutine b1AssgnB2 (b1, b2)
        class (base(4)), intent(out) :: b1(:) ! tcx: (4)
        class (base(4)), intent(in) :: b2(:) ! tcx: (4)

        if (.not. same_type_as (b1, b2)) error stop 10_4

        if (size (b1) /= size(b2)) return

        do i = 1, size (b2)
            if (associated (b2(i)%id)) allocate (b1(i)%id, source=b2(i)%id)

            select type (b1)
                type is (base(4)) ! tcx: (4)
                type is (child(4,*)) ! tcx: (4,*)
                    select type (b2)
                        type is (child(4,*)) ! tcx: (4,*)
                            if (associated (b2(i)%name)) &
                                    allocate (b1(i)%name, source=b2(i)%name)
                        class default
                            error stop 14_4
                    end select
                class default
                    error stop 15_4
            end select
        end do
    end subroutine
end module

program ffinal507k
use m
    class (base(4)), allocatable :: b1(:), b2(:) ! tcx: (4)

    allocate (child(4,20) :: b1(1:0), b2(2)) ! tcx: (4,20)

    allocate (b2(1)%id, b2(2)%id)

    b1 = b2

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 5 changes
