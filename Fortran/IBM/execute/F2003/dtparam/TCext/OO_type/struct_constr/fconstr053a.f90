! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr053a.f
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
!*  DATE                       : 04/22/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : structure constructor (structure constructor
!                               overloaded by the generic function; test the use
!                               of the generic name in the structure constructor
!                               in the extended type)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type container(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        private
        class(*), pointer :: data(:,:) => null()

        contains

        procedure :: print => printContainer
        procedure :: clear => freeData
    end type

    interface container
        module procedure makeContainerData1
        module procedure makeContainerData2
    end interface

!    private makeContainerData1, makeContainerData2
    contains

    subroutine printContainer (co)
        class (container(4,*)), intent(in) :: co

        if (associated (co%data)) then
            do j = lbound(co%data, 2), ubound(co%data, 2)
                do i = lbound(co%data,1), ubound(co%data,1)
                    call printItem (co%data(i, j))
                end do
            end do
        end if
    end subroutine

    subroutine printItem (x)
        class(*), intent(in) :: x

        select type (x)
            type is (integer)
                print *, x
            type is (character(*))
                print *, x
            type is (real)
                write (*, '(f12.3)') x
            class default
                print *, 'not a predefined type'
        end select
    end subroutine

    subroutine freeData (co)
        class (container(4,*)), intent(inout) :: co

        if (associated(co%data)) deallocate(co%data)
    end subroutine

    type (container(4,20)) function makeContainerData1 (x1, ishape)
        class(*), intent(in) :: x1(:)
        integer, intent(in) :: ishape (2)

        allocate (makeContainerData1%data(ishape(1), ishape(2)), &
                    source=reshape(x1, ishape))
    end function

    type (container(4,20)) function makeContainerData2 (x2)
        class (*), intent(in) :: x2(:,:)

        allocate (makeContainerData2%data (size(x2,1), size(x2,2)), source=x2)
    end function
end module

module m1
use m
    type, extends(container) :: namedContainer(k2,n2)    ! (4,20,1,20)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: name

        contains

        procedure :: print => printNamedContainer
    end type

    contains

    subroutine printNamedContainer (co)
        class (namedContainer(4,*,1,*)), intent(in) :: co

        write (*, *) 'data part:'

        call co%container%print

        print *, 'and Name = ', co%name
    end subroutine
end module

program fconstr053a
use m1
    class (container(4,20)), allocatable :: co1

    character (5), dimension (3,2) :: c1

    !! first test
    allocate (co1, source=namedContainer(4,20,1,20)( &
                container=container((/1,2,3,4/), (/2,2/)), name='test 1'))

    call co1%print

    !! second test
    c1(:,1) = (/'ibm', 'xlf', 'com'/)
    c1(:,2) = (/'abc', 'ibm', 'xyz'/)

    associate (x => namedContainer(4,20,1,20)(name = 'test 2', &
                container = makeContainerData2(c1)))

        call x%print
    end associate
end
