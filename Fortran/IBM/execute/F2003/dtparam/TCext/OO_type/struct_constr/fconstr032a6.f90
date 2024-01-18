! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr032a6.f
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
! %GROUP: fconstr032a6.f
! %VERIFY: fconstr032a6.out:fconstr032a6.vf
! %STDIN:
! %STDOUT: fconstr032a6.out
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
!*  DATE                       : 10/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (two dimensional
!                               poly-allocatable array component)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure, non_overridable :: getID
        procedure :: print => printBase
        procedure :: compCount => baseCompCount
    end type

    contains

    elemental integer(4) function getID (b)
        class(base(4)), intent(in) :: b

        getID = b%id
    end function

    elemental integer(4) function baseCompCount (b)
        class(base(4)), intent(in) :: b

        baseCompCount = 1
    end function

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m
    type container(k2,n1)    ! (4,20)
        integer, kind                :: k2
        integer, len                 :: n1
        class(base(k2)), allocatable :: data (:,:)
    end type
end module


module m2
use m
    type, extends(base) :: child(k3,n2,k4)    ! (4,1,15,4)
        integer, kind             :: k3,k4
        integer, len              :: n2
        character(kind=k3,len=n2) :: name
        logical(k4)               :: set

        contains

        procedure :: print => printChild
        procedure :: compCount => childCompCount
    end type

    contains

    subroutine printChild (b)
        class (child(4,1,*,4)), intent(in) :: b

        print *, b%id, b%name, b%set
    end subroutine

    elemental integer(4) function childCompCount (b)
        class (child(4,1,*,4)), intent(in) :: b

        childCompCount = 3
    end function
end module


program fconstr032a6
use m1
use m2
    class (base(4)), allocatable :: b1(:,:)
    integer(4) idVal(2,2)   !<-- this is to be used for validation of ids

    allocate (b1(2, -1:0), source=reshape ((/(child(4,1,15,4) (i, 'test', .true.), &
                                    i = 1, 4)/), (/2,2/)))


    idVal(:,1) = (/1,2/)
    idVal(:,2) = (/3,4/)

    if (any (b1%getID() /= idVal)) error stop 1_4

    if (any (b1%compCount() /= 3)) error stop 2_4

    do i = 1, 2
        do j = -1, 0
            call b1(i,j)%print
        end do
    end do

    call associate (x = container(4,20) (data=b1))
    contains

!    associate (x => container(4,20) (data=b1))
    subroutine associate (x)
        type(container(4,*)), intent(in) :: x

        if (.not. allocated (x%data)) error stop 3_4

        if (any (lbound(x%data) /= (/1, -1/))) error stop 4_4
        if (any (ubound(x%data) /= (/2, 0/))) error stop 5_4

        if (any (x%data%getID() /= idVal)) error stop 6_4

        if (any (x%data%compCount() /= 3)) error stop 7_4

        do i = 1, 2
            do j = -1, 0
                call x%data(i,j)%print
            end do
        end do
    end subroutine
end
