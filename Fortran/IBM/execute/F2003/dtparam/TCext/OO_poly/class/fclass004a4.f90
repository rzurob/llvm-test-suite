! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass004a4.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass004a4.f
! %VERIFY: fclass004a4.out:fclass004a4.vf
! %STDIN:
! %STDOUT: fclass004a4.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (combination of structure
!                               component, array and intrinsic assignment)
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
    type base(k1)    ! (8)
        integer, kind            :: k1
        integer(k1), allocatable :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase(b)
        class (base(8)), intent(in) :: b

        select type (b)
            type is (base(8))
                if (allocated (b%id)) then
                    print *, b%id
                else
                    print *, 'id not allocated'
                end if
            class default
                print *, 'incorrect type'
        end select
    end subroutine

    subroutine printChild(b)
        class (child(8,1,*)), intent(in) :: b

        select type (b)
            type is (child(8,1,*))
                if (allocated (b%id)) then
                    print *, b%id, b%name
                else
                    print *, 'id not allocated;', b%name
                end if
            class default
                print *, 'wrong type'
        end select
    end subroutine
end module

module m1
use m
    type container(k3)    ! (8)
        integer, kind            :: k3
        class(base(k3)), pointer :: data(:) => null()

        contains

        procedure :: print => printData
    end type

    contains

    subroutine printData (co)
        class (container(8)), intent(in) :: co

        print *, 'bounds:', lbound(co%data,1), ubound(co%data,1)

        do i=lbound(co%data,1), ubound(co%data,1)
            call co%data(i)%print
        end do
    end subroutine
end module

program fclass004a4
use m1
    type (container(8)) :: co1(10)

    class (base(8)), target, allocatable :: b1(:, :)

    integer(8) k

    allocate (b1(5,2), source=reshape((/(child(8,1,20)(k, 'xlftest team'), k=1,10)/), &
                                    (/5,2/)))

    !! assign the first 5 elements of co1

    forall (i=1:5) co1(i) = container(8)(b1(i,:))

    !! assign the last 5 elements of co1
    co1(10:6:-1) = co1(1:5)


    !! verify the results

    do i = 1, 10
        call co1(i)%print
    end do
end
