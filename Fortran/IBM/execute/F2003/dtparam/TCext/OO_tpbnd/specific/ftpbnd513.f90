! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd513.f
! opt variations: -qck -qnol -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   : specific type bound (type-bound functon
!*                               reference via array element)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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

        procedure, non_overridable :: getID => baseID
    end type

    type, extends (base) :: child    ! (20,4)
        character(n1) :: name

        contains

        procedure, non_overridable :: getName => ChildName
    end type

    contains

    integer*4 function baseID (b)
        class (base(*,4)), intent(in) :: b

        baseID = b%id
    end function

    character(20) function ChildName (c)
        class (child(*,4)), intent(in) :: c

        ChildName = c%name
    end function

    character*2 function int2Char (i)
        integer, intent(in) :: i

        write (int2Char, '(i2)') i
    end function
end module

program ftpbnd513
use m
    type (child(20,4)) :: c1(10)

    c1%base = (/(base(20,4)(i), i=10,19)/)

    c1%name = (/('c1_'//int2Char(c1(i)%id), i=1,10)/)

    do i = 1, 10
        if (c1(i)%getID() /= 9+i) error stop 1_4
        print *, c1(i)%getName()
    end do

end
