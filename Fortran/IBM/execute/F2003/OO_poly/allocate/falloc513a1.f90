!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/06/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocate (allocation of zero-size arrays with
!                               source-expr of scalar)
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
    type base
        integer(8), allocatable :: i1(:)

        real(8), pointer :: r1 => null()
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'
    end type
end module

program falloc513a
use m
    class (base), pointer :: x1(:)
    class (*), allocatable :: b1(:,:)

    allocate (b1(3, 3:1), source=child((/1_8, 2_8/)))
    allocate (x1(1:0), source=child ((/100_8/), name='x1'))

    if ((.not. associated (x1)) .or. (.not. allocated (b1))) error stop 1_4

    if ((size(x1) /= 0) .or. (size(b1) /= 0)) error stop 2_4

    if ((lbound(x1,1) /= 1) .or. (ubound(x1,1) /= 0)) error stop 3_4

    if (any(lbound(b1) /= 1) .or. any(ubound(b1) /= (/3,0/))) error stop 4_4

    select type (y=>b1)
        class is (base)
            select type (y)
                type is (child)
                    if (size (y) /= 0) error stop 5_4

                    if (any (shape (y) /= (/3,0/))) error stop 6_4
                class default
                    error stop 9_4
            end select
        class default
            error stop 10_4
    end select
end
