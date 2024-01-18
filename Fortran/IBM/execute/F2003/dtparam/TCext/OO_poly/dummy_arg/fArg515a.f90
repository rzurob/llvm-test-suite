! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg515a.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg515a.f
! %VERIFY: fArg515a.out:fArg515a.vf
! %STDIN:
! %STDOUT: fArg515a.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/17/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
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
    type base(k1,k2,n1)    ! (4,1,20)
        integer, kind             :: k1,k2
        integer, len              :: n1
        integer(k1)               :: id
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printBase
    end type

    type container(k3,k4)    ! (4,1)
        integer, kind                     :: k3,k4
        class(base(k3,k4,:)), allocatable :: data (:)

        contains

        procedure :: print => printWithSort
    end type

    contains

    subroutine printBase (b)
        class (base(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    !! this routine uses an input comparison function for sorting
    !! what this does is similar to the sort() algorithm in C++'s STL
    subroutine printWithSort (co, less)
        class (container(4,1)), intent(in) :: co

        interface
            logical function less (b1, b2)
                import base
                class (base(4,1,*)), intent(in) :: b1, b2
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
        class (base(4,1,*)), intent(in) :: b1, b2

        noCompare = .false.
    end function

    logical function compareID (b1, b2)
        class (base(4,1,*)), intent(in) :: b1, b2

        compareID = (b1%id < b2%id)
    end function

    logical function compareIDThenName (b1, b2)
        class (base(4,1,*)), intent(in) :: b1, b2

        if (b1%id == b2%id) then
            compareIDThenName = (b1%name < b2%name)
        else
            compareIDThenName = (b1%id < b2%id)
        end if
    end function

    logical function compareName (b1, b2)
        class (base(4,1,*)), intent(in) :: b1, b2

        compareName = (b1%name < b2%name)
    end function
end module

program fArg515a
use m
use m1
    type (container(4,1)) :: co1

    allocate (base(4,1,20) :: co1%data(5))

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
