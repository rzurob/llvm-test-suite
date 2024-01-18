! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_tpbnd/specific/ftpbnd518a.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (type bound as actual
!*                               argument, specification expression)
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
        integer(k1)      id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printData (b, lb, ub)
        integer, intent(in) :: lb, ub
        class (base(*,4)), intent(in) :: b(lb:ub)

        do i = lb, ub
            call b(i)%print
        end do
    end subroutine
end module

module m1
use m, only : base, child
    type dataType(k3,n3)    ! (4,20)
        integer, kind              :: k3
        integer, len               :: n3
        class(base(:,k3)), pointer :: data (:) => null()

        contains

        procedure :: lbound => lowerBound
        procedure :: ubound => upperBound
    end type

    contains

    integer function lowerBound (d)
        class (dataType(4,*)), intent(in) :: d

        lowerBound = lbound (d%data, 1)
    end function

    integer function upperBound (d)
        class (dataType(4,*)), intent(in) :: d

        upperBound = ubound (d%data, 1)
    end function
end module

program ftpbnd518a
use m
use m1, only : dataType
    type (dataType(4,20)) :: d1

    type (base(20,4)), target :: b1 (-1:4)
    type (child(20,4,4,20)), target :: c1 (3:5)

    b1 = (/(base(20,4) (id = i), i=-1,4)/)

    c1 = (/(child(20,4,4,20)(id=i, name='c1_'//char(ichar('0')+i)), i=3,5)/)

    d1 = dataType(4,20) (b1)

    call printData (d1%data, d1%lbound(), d1%ubound())

    d1 = dataType(4,20) (data = c1)

    call printData (d1%data, d1%lbound(), d1%ubound())
end
