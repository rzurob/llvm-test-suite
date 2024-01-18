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
!*  DATE                       : 02/24/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : specific type bound (type bound procedures used
!                               in select type)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        private
        integer(4) id

        contains

        procedure :: setID => assignBaseID
        procedure :: getID => getBaseID
    end type

    contains

    elemental subroutine assignBaseID (b, id)
        intent(in) :: id
        class (base), intent(inout) :: b

        b%id = id
    end subroutine

    elemental integer(4) function getBaseID (b)
        class(base), intent(in) :: b

        getBaseID = b%id
    end function
end module

module m1
use m, only : base
    type, extends(base) :: child
        private
        character(20) :: name

        contains

        procedure :: setName => assgnChildName
        procedure :: getName => getChildName
    end type

    contains

    elemental subroutine assgnChildName (c, name)
        class (child), intent(inout) :: c
        character(*), intent(in) :: name

        c%name = name
    end subroutine

    elemental character(20) function getChildName (c)
        class (child), intent(in) :: c

        getChildName = c%name
    end function
end module


module n
use m1, only : base, child

    contains

    subroutine printBaseArray (b)
        class (base), intent(in) :: b(:,:)

        do i = 1, size(b, 1)
            do j = 1, size(b, 2)
                select type (x => b(i,j))
                    type is (base)
                        print *, x%getID()
                    type is (child)
                        print *, x%getID(), x%getName()
                    class default
                        error stop 10_4
                end select
            end do
        end do
    end subroutine
end module

program ftpbnd524
use n
    integer(4), allocatable :: i1(:,:)
    character(20), allocatable :: c1(:,:)

    class(base), allocatable :: b1(:,:)

    allocate (i1(2,2), source=reshape((/1,2,3,4/), (/2,2/)))
    allocate (c1(2,2), source=reshape((/'test 1','test 2','test 3','test 4'/), &
                    (/2,2/)))

    allocate (child:: b1(0:1, 2))

    call b1%setID (i1)

    select type (b1)
        type is (child)
            call b1%setName(c1)
        class default
            error stop 1_4
    end select

    call printBaseArray (b1)
end
