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
!*  DATE                       : 10/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test MERGE for derived type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8), allocatable :: id
    end type

    type, extends(base) :: child
        character(:), allocatable :: name
    end type

    interface base
        procedure baseObjWithDefault
    end interface

    interface child
        procedure childObjWithDefault
    end interface

    contains

    type(base) function baseObjWithDefault ()
        allocatable baseObjWithDefault

        allocate (baseObjWithDefault)
        baseObjWithDefault%id = -1
    end function

    type(child) function childObjWithDefault ()
        allocatable childObjWithDefault

        allocate (childObjWithDefault)
        childObjWithDefault%base = base()

        childObjWithDefault%name = 'no-name'
    end function
end module

module m1
use m
    type container
        type(base), allocatable :: b
        type(child), allocatable :: c
    end type

    interface container
        procedure containerObjWithDefault
    end interface

    contains

    type(container) function containerObjWithDefault ()
        allocatable containerObjWithDefault

        allocate (containerObjWithDefault)
        containerObjWithDefault%b = base()
        containerObjWithDefault%c = child()
    end function
end module

program intrinsic005
use m1
    type(container), allocatable :: co1(:,:), co2(:,:)

    logical(8), allocatable :: l1(:,:)

    allocate(co1(10, 50), l1(10, 50))

    do i = 1, 10
        do j = 1, 50
            co1(i,j) = container()

            co1(i,j)%b%id = i*j

            co1(i,j)%c%id = i**(j/5)
            co1(i,j)%c%name = 'xlftest'


            l1(i,j) = co1(i,j)%c%id > co1(i,j)%b%id
        end do
    end do

    co2 = merge (container(), co1, l1)

    !! verify co2
    do i = 1, 10
        do j = 1, 50
            if (l1(i,j)) then
                if (co2(i,j)%b%id /= -1) error stop 1_4

                if ((co2(i,j)%c%id /= -1) .or. &
                    (co2(i,j)%c%name /= 'no-name')) error stop 2_4
            else
                if (co2(i,j)%b%id /= i*j) error stop 3_4

                if ((co1(i,j)%c%id /= i**(j/5)) .or. &
                    (co1(i,j)%c%name /= 'xlftest')) error stop 4_4
            end if
        end do
    end do
end
