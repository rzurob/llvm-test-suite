! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr051.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr051.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (poly-entities used as
!                               the data-source for nonpoly scalar component)
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
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type
end module

program fconstr051
use m
    type container(k3)    ! (4)
        integer, kind  :: k3
        type(base(k3)) :: b1
    end type

    class (base(4)), allocatable :: b1
    class (base(4)), pointer :: b2(:)

    type (child(4,1,15)), target :: c1(2:10)

    b2 => c1
    allocate (b1, source=child(4,1,15)(1,'b1'))

    c1%id = (/(i, i=2,10)/)
    c1%name = 'c1_array_of_9'

    associate (x1 => container(4) (b1), x2 => container(4) (b2(3)))
        if ((x1%b1%id /= 1) .or. (x2%b1%id /= 3)) error stop 1_4
    end associate
end
