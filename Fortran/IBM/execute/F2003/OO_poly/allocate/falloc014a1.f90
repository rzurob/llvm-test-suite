!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc014a1.f
! %VERIFY: falloc014a1.out:falloc014a1.vf
! %STDIN:
! %STDOUT: falloc014a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (allocate and deallocate can be
!                               invoked during the intrinsic assignment)
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
        integer(4) :: data

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15), allocatable :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%data
    end subroutine


    subroutine printChild (b)
        class (child), intent(in) :: b

        if (allocated(b%name)) then
            print *, b%data, b%name
        else
            print *, b%data
        end if
    end subroutine
end module

module m1
use m
    type dataType
        class (base), allocatable :: data(:,:,:)

        contains

        procedure :: print => printData
    end type

    contains

    subroutine printData (d)
        class (dataType), intent(in) :: d

        if (allocated(d%data)) then
            print *, 'lbounds of data are', lbound(d%data)
            print *, 'ubounds of data are', ubound(d%data)

            do i3 = lbound(d%data,3), ubound(d%data,3)
                do i2 = lbound(d%data,2), ubound(d%data,2)
                    do i1 = lbound(d%data,1), ubound(d%data,1)
                        print *, 'element:', i1, i2, i3
                        call d%data(i1,i2,i3)%print
                    end do
                end do
            end do
        else
            print *, 'data in dataType not allocated'
        end if
    end subroutine
end module

program falloc014a1
use m1
    type(child) :: c1(2,2,2)

    type (dataType) :: d1, d2

    c1 = reshape ((/(child(i, 'c1'), i = 1, 8)/), (/2,2,2/))

    allocate (d2%data(-1:0,0:1,2), source=c1)

    call d2%print

    !! now the intrinsic assignment will get d1's data allocated

    print *, ''
    print *, 'intrinsic assignment'
    print *, ''

    d1 = d2

    call d1%print

    !! deallocate both data from d1 and d2

    deallocate (d2%data)

    d1 = d2

    call d1%print
end
