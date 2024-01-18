! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/zeroSize/zeroSizeArray009.f
! opt variations: -qnol -qnodeferredlp

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
!*  DATE                       : 08/30/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use a zero-sized array from the variable; also
!                               test the auto-deallocation of variable at the
!                               assignment.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind              :: k1
        integer, len               :: n1
        real(k1), pointer, private :: data(:) => null()

        contains

        final :: finalizeBase
        procedure :: nullifyData
    end type

    interface base
        procedure genBaseObj
    end interface

    contains

    subroutine nullifyData (b)
        class (base(*,8)), intent(inout) :: b

        nullify(b%data)
    end subroutine

    subroutine finalizeBase (b)
        type(base(*,8)), intent(inout) :: b

        write (*, '(a)', advance='no') 'in finalizeBase; '

        if (associated(b%data)) then
            write (*, '(a, (10g12.6))') 'b%data = ', b%data
            deallocate (b%data)
        else
            print *, ''
        end if
    end subroutine

    type(base(20,8)) function genBaseObj (r1)
        real(8), intent(in) :: r1(:)

        allocate (genBaseObj%data(size(r1)), source=r1)
    end function
end module

module n
use m, only : base
    type container(n2,k2)    ! (20,4)
        integer, kind            :: k2
        integer, len             :: n2
        integer(k2), allocatable :: id
        class(*), allocatable :: data
    end type
end module

program zeroSizeArray009
use n
    type (container(:,4)), allocatable :: co1(:)

    co1 = (/(container(20,4)(i, i), container(20,4)(i, null()), &
        container(20,4)(i, base((/(j*1.0d0, j=1,i)/))), i=1,3)/)

    if (.not. allocated(co1)) error stop 1_4

    if (size(co1) /= 9) error stop 2_4

    print *, 'assignment'

    do i = 3, 9, 3
        select type (x => co1(i)%data)
            type is (base(*,8))
                call x%nullifyData ! this is a dangling pointer; nullify

            class default
                stop 10
        end select
    end do

    co1 = co1(::-1)

    print *, 'end'

    if (.not. allocated(co1)) error stop 4_4
    if (size(co1) /= 0) error stop 5_4
end
