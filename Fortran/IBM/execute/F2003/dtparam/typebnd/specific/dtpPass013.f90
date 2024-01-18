!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/19/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Type bound is a derived
!                               type with deferred type parameter.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real data(n)

        contains

        procedure :: screen => getDataOverInput

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b1)
        class(base(*)), intent(in) :: b1

        write (*, '(6g12.5)') b1%data
    end subroutine

    class(base(:)) function getDataOverInput (b1, d1)
        class(base(*)), intent(in) :: b1
        real, intent(in) :: d1

        allocatable :: getDataOverInput

        real local(b1%n)
        integer itotal

        itotal = 0

        do i = 1, b1%n
            if (b1%data(i) > d1) then
                itotal = itotal + 1

                local(itotal) = b1%data(i)
            end if
        end do

        allocate (base(itotal) :: getDataOverInput)

        getDataOverInput%data(:) = local(:itotal)
    end function
end module


module m1
use m
    type, extends(base) :: child (len)
        integer, len :: len

        character(len) :: IndexName (n)

        contains

        procedure :: screen => getChildOverInput

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b1)
        class(child(*,*)), intent(in) :: b1

        do i = 1, b1%n
            write (*, '(g12.5, a)') b1%data(i), b1%indexName(i)
        end do
    end subroutine


    class(base(:)) function getChildOverInput (b1, d1)
        class(child(*,*)), intent(in) :: b1
        real, intent(in) :: d1

        allocatable :: getChildOverInput

        real localReal(b1%n)
        character(len = b1%len) localStr(b1%n)
        integer itotal

        itotal = 0

        do i = 1, b1%n
            if (b1%data(i) > d1) then
                itotal = itotal + 1

                localReal(itotal) = b1%data(i)
                localStr(itotal) = b1%IndexName(i)
            end if
        end do

        allocate (getChildOverInput, source=child(n=itotal,len=b1%len) &
                (localReal(:itotal), localStr(:itotal)))
    end function getChildOverInput
end module


program dtpPass013
use m1
    type(base(:)), allocatable :: b1
    type(child(:,:)), allocatable :: c1
    class(base(:)), allocatable :: b2
    class(child(:,:)), allocatable :: c2

    type (base(100)) :: b3

    type(child(150, 10)) c3

    b3%data = [(i, i = 1, 100)]

    b1 = b3%screen(60.)

    allocate (b2, source = b1%screen (80.0))

    print *, 'b1%print'
    call b1%print
    print *

    print *, 'b2%print'
    call b2%print
    print *

    c3%data(:) = [(i, i = 1, 150)]

    do i = 1, 150
        write(c3%indexName(i), '(a, i3.3, a)') 'data(', i, ')'
    end do


    call foo (c3%screen (120.))
!    select type (x => c3%screen (120.))
!        type is (child(*,*))
!            allocate (c2, source = x)
!
!            select type (y => c2%screen (145.))
!                type is (child(*,*))
!                    c1 = y
!
!                class default
!                    stop 20
!            end select
!
!        class default
!            stop 10
!    end select

    print *, 'c1%print'
    call c1%print
    print *

    print *, 'c2%print'
    call c2%print
    print *


    deallocate (b2)
    allocate (b2, source = c2%screen (1.0))

    print *, 'last print'
    call b2%print

    contains

    subroutine foo1 (y)
        class(base(*)), intent(in) :: y

        select type (y)
            type is (child(*,*))
                c1 = y

            class default
                stop 20
        end select
    end subroutine

    subroutine foo (x)
        class(base(*)), intent(in) :: x

        select type (x)
            type is (child(*,*))
                allocate (c2, source = x)

                call foo1(c2%screen (145.))

            class default
                stop 10
        end select
    end subroutine
end
