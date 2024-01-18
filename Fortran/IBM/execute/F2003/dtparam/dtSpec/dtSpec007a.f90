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
!*  DATE                       : 02/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: C481, part 1: The assumed-type-parameter
!                               for dummy-arg, nonpointer, nonallocatable
!                               dummy-arg, assumed-shape array.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type pair (l)
        integer, len :: l

        real :: data = -1.0
        character(l) :: desc = 'default'
    end type

    type base (n, l)
        integer, len :: n, l

        real :: data (n)
        character(l) :: desc (n)

        contains

        procedure :: update => setBaseVal
    end type

    contains

    subroutine setBaseVal (b, p)
        class(base(*,*)), intent(inout) :: b
        type (pair(l=b%l)), intent(in) :: p(b%n)

        b%data = p%data
        b%desc = p%desc
    end subroutine

    subroutine printBaseArray (b)
        class(base(n=*,l=*)), intent(in) :: b(:)

        do i = 1, size(b)
            print *, 'base element:', i, new_line('a')

            do j = 1, b%n
                write (*, 100) b(i)%data(j), b(i)%desc(j)
            end do
        end do

100     format ('data = ', f12.2, '; description: ', a)
    end subroutine
end module

program dtSpec007a
use m
    class(base(:,:)), pointer :: b1(:)
    type (pair(:)), allocatable :: p1(:,:)

    allocate (pair(l=20):: p1(15, 5))

    allocate (base(l=p1%l, n=size(p1, 1)) :: b1(size(p1,2)))

    do j = 1, 5
        do i = 1, 15
            p1(i,j)%data = i*1.e1 + j*1.0
            p1(i,j)%desc = convertNumber(i, j)
        end do
    end do

    do i = 1, 5
        call b1(i)%update(p1(:,i))
    end do

    !! let's print out the values
    call printBaseArray (b1)

    contains

    character(20) function convertNumber (i1, i2) result (string)
        integer, intent(in) :: i1, i2

        write (string, '("elemenet: ", i2, ", ", i1)') i1, i2
    end function
end

