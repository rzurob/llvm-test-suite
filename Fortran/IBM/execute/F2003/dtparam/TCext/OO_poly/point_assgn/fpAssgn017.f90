! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodefaultpv -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn017.f
! opt variations: -qnock -qnol -qdefaultpv -qdeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn017.f
! %VERIFY: fpAssgn017.out:fpAssgn017.vf
! %STDIN:
! %STDOUT: fpAssgn017.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (structure component
!*                               used to contain different types)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    private printBase, printChild

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, 'Base: id =', b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, 'Child: id =', b%id, 'name =', b%name
    end subroutine
end module

module m1
use m
    type dataType(n2,k3)    ! (20,4)
        integer, kind               :: k3
        integer, len                :: n2
        private
        class(base(n2,k3)), pointer :: data => null()

        contains

        procedure :: assgn => AssgnData
        procedure :: print => printDataType
    end type

    type (dataType(20,4)), save :: d1 (10)

    private AssgnData, printDataType

    contains

    subroutine printDataType (d)
        class (dataType(*,4)), intent(in) :: d

        call d%data%print
    end subroutine

    subroutine AssgnData (d, d1)
        class (dataType(*,4)), intent(inout) :: d
        class (base(*,4)), intent(in), target :: d1

        d%data => d1
    end subroutine
end module

program fpAssgn017
use m1

    type (child(20,4,1)), target :: c1(5)
    type (base(20,4)), target :: b1(5)

    b1 = (/(base(20,4)(id = i), i=1,5)/)

    c1 = (/(child(20,4,1) (id = i, name = 'c1'), i=6,10)/)

    do i = 1, 5
        call d1(2*i-1)%assgn(b1(i))
        call d1(2*i)%assgn(c1(i))
    end do

    do i = 1, 10
        call d1(i)%print
    end do
end
