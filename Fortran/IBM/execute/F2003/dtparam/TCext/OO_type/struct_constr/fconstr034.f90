! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr034.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable scalar component; use derived
!                               type for the data-source)
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
    type container(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data
    end type
end module

module n
    type base(k2)    ! (8)
        integer, kind :: k2
        integer(k2)   :: id
    end type

    type, extends(base) :: child(k3,n2)    ! (8,1,18)
        integer, kind             :: k3
        integer, len              :: n2
        character(kind=k3,len=n2) :: name
    end type
end module

program fconstr034
use n
use m
    class(*), allocatable :: x1
    class (*), pointer :: x2(:)

    type (child(8,1,18)), target :: c1 (2:10)

    allocate (x1, source=child(8,1,18)(100, 'x1'))

    c1%id = (/(i,i=2,10)/)
    c1%name = 'c1_array_of_9'

    x2 => c1

    associate (y1 => container(4,20)(x1), y2 => container(4,20)(x2(5)))
        select type (z => y1%data)
            type is (child(8,1,*))
                print *, 'child type:', z
            type is (base(8))
                print *, 'base type:', z
            class default
                print *, 'wrong'
        end select

        select type (z => y2%data)
            class is (base(8))
                print *, 'class base'
            type is (child(8,1,*))
                print *, 'child:', z
            class default
                print *, 'wrong'
        end select
    end associate
end
