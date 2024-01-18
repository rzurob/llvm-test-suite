! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_tpbnd/specific/ftpbnd522.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2005
!*
!*  DESCRIPTION                : specific type bound (select type usage in the
!                               type bound)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(n1,k1)    ! (20,8)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      x, y

        contains

        procedure :: print => printPoint
    end type

    type, extends(point) :: colorPoint    ! (20,8)
        integer(selected_int_kind(3)) color
    end type

    contains

    subroutine printPoint (p)
        class (point(*,8)), intent(in) :: p

        select type (p)
            type is (point(*,8))
                print *, p%x, p%y
            class default
                print *, 'print operation not defined'
        end select
    end subroutine
end module

program ftpbnd522
use m
    class (point(:,8)), allocatable :: p1(:)

    allocate (p1(3), source=(/colorPoint(20,8)(10, 20, 2), colorPoint(20,8)(30, 40, 4), &
                        colorPoint(20,8) (1, 5, 2)/))

    call p1(2)%print

    select type (p1)
        type is (colorPoint(*,8))
            associate (x => p1%point)
                call x(1)%print
                call x(2)%print
                call x(3)%print
            end associate
        class default
            error stop 1_4
    end select
end
