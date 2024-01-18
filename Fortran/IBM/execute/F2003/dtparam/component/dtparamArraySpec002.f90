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
!*  DATE                       : 01/24/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.1: array component)
!                               Case: component-array-spec overrides the
!                               specification in DIMENSION attribute; apply to
!                               type-parameters used as the bounds specifiers;
!                               for 2-dimensional array components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (dim1, dim2)
        integer, len :: dim1, dim2

        real, dimension(dim1, dim2) :: data
        real, private :: reshape(dim1*dim2)

        contains

        procedure :: set => setReshape
        procedure :: get => getReshape
    end type

    contains

    subroutine setReshape (b)
        class(base(*,*)), intent(inout) :: b

        b%reshape = reshape (b%data, shape(b%reshape))
    end subroutine

    real function getReshape (b)
        class (base(*,*)), intent(in) :: b
        dimension getReshape(b%dim1*b%dim2)

        getReshape = b%reshape
    end function
end module

program dtparamArraySepc002
use m
    type (base(10, 3)) b1
    type (base(:,:)), allocatable :: b2(:)

    real, pointer :: r1(:)
    real, allocatable :: r2(:), r3(:,:)

    logical(4), external :: precision_r4

    allocate (r3(10, 3), source=reshape((/((1.0e1*i+j, i= 1, 10), j = 1, 3)/),&
            (/10, 3/)))

    b1%data = r3

    call b1%set()

    allocate (r1(b1%dim1*b1%dim2), source=b1%get())

    deallocate (r3)

    allocate (r3(5, 4), source=reshape((/((1.0e1*i+j, i=1,5), j=1,4)/), &
            (/5,4/)))

    allocate (base(size(r3,1), size(r3, 2)) :: b2(2))

    b2(1)%data = r3
    b2(2)%data = -1.0e0

    deallocate (r3)

    allocate(r3(20, 2))

    call b2(1)%set
    call b2(2)%set()

    r3(:,1) = b2(1)%get()
    r3(:,2) = b2(2)%get()


    !! verify r1 and r3
    allocate (r2(30), source=(/((1.0e1*i+j, i= 1, 10), j = 1, 3)/))

    do i = 1, 30
        if (.not. precision_r4 (r1(i), r2(i))) error stop 1_4
    end do

    deallocate (r2)

    allocate (r2(20))
    r2 = (/((1.0e1*i+j, i=1,5), j=1,4)/)

    do i = 1, 20
        if (.not. precision_r4(r2(i), r3(i,1))) error stop 2_4

        if (.not. precision_r4(r3(i,2), -1.0e0)) error stop 3_4
    end do
end
