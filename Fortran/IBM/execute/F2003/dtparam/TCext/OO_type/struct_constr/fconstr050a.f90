! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr050a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr050a.f
! %VERIFY: fconstr050a.out:fconstr050a.vf
! %STDIN:
! %STDOUT: fconstr050a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (nonpoly allocatable
!                               array component with poly-array data-source; use
!                               rank-one arrays; use associate construct)
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
        integer(k1)      id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1)    name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type container(k3)    ! (4)
        integer, kind               :: k3
        type(base(k3)), allocatable :: data(:)
    end type
end module

program fconstr050a
use m1
    class (base(4)), allocatable :: b1(:)

    allocate (b1(-1:0), source = (/child(4,1,15)(1,'test1'), child(4,1,15)(2,'test2')/))

    call foo (x = container(4) (b1))

    contains

!    associate (x => container(4) (b1))
    subroutine foo (x)
        type(container(4)), intent(in) :: x

        if (.not. allocated (x%data)) error stop 1_4

        if ((lbound(x%data,1) /= -1) .or. (ubound(x%data,1) /= 0)) error stop 2_4
        print *, x%data

        call x%data(-1)%print
        call x%data(0)%print
    end subroutine
end
