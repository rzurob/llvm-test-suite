! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy-procedure as the
!*                               argument in the type-bound)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id
        character*20 :: name

        contains

        procedure :: print => printBase
    end type

    type container
        class (base), allocatable :: data (:)

        contains

        procedure :: print => printWithSort
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    !! this routine uses an input comparison function for sorting
    !! what this does is similar to the sort() algorithm in C++'s STL
    subroutine printWithSort (co, less)
        class (container), intent(in) :: co

        interface
            logical function less (b1, b2)
                import base
                class (base), intent(in) :: b1, b2
            end function
        end interface

        !! we use an array for indexing the data's ordering
        integer*4, allocatable :: index (:)

        if (allocated (co%data)) then
            allocate (index(size(co%data)))

            index = (/(i,i=1,size(index))/)


            !! we use bubble sort to order the index
            do j = size(index), 1, -1
                do k = 2, j
                    if (less(co%data(index(k)), co%data(index(k-1)))) then
                        kk = index (k)

                        index (k) = index(k-1)
                        index(k-1) = kk
                    end if
                end do
            end do

            !! print the array
            do i = 1, size (co%data)
                call co%data(index(i))%print
            end do
        end if
    end subroutine
end module


module m1
use m, only : base

    contains

    logical function noCompare (b1, b2)
        class (base), intent(in) :: b1, b2

        noCompare = .false.
    end function

    logical function compareID (b1, b2)
        class (base), intent(in) :: b1, b2

        compareID = (b1%id < b2%id)
    end function

    logical function compareIDThenName (b1, b2)
        class (base), intent(in) :: b1, b2

        if (b1%id == b2%id) then
            compareIDThenName = (b1%name < b2%name)
        else
            compareIDThenName = (b1%id < b2%id)
        end if
    end function

    logical function compareName (b1, b2)
        class (base), intent(in) :: b1, b2

        compareName = (b1%name < b2%name)
    end function
end module

program fArg515a
use m
use m1
    type (container) :: co1

    allocate (co1%data(5))

    co1%data%id = (/3, 2, 3, 8, 1/)
    co1%data%name = (/'test', 'T1T2', 'abcd', 'high', 'unit'/)

    print *, 'test1-------------------------------'

    !! no sorting is done, print as is
    call co1%print (noCompare)

    print *, 'test2-------------------------------'

    !! sort the data based on ID values
    call co1%print (compareID)

    print *, 'test3-------------------------------'

    !! sort the data based on IDs, then 2nd sort based on names
    call co1%print (compareIDThenName)


    !! sort the printout based on names
    print *, 'test4-------------------------------'

    call co1%print (compareName)
end