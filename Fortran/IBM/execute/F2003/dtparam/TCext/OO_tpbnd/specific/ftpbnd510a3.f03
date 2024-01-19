! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/OO_tpbnd/specific/ftpbnd510a3.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (named constants can be of
!                           derived type with pointer or allocatable component)
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
    type node(n1,k1)    ! (20,4)
        integer, kind              :: k1
        integer, len               :: n1
        integer(k1)                :: value
        class(node(:,k1)), pointer :: next => null()

        contains

        procedure :: print => printNode
    end type

    type dataType(k2,n2)    ! (4,20)
        integer, kind                  :: k2
        integer, len                   :: n2
        class(node(:,k2)), allocatable :: data(:)

        contains

        procedure :: print => printDataType
    end type

    contains

    subroutine printNode (n)
        class (node(*,4)), intent(in) :: n

        if (associated (n%next)) then
            print *, n%value, 'next node exist'
        else
            print *, n%value, 'end of list'
        end if
    end subroutine

    subroutine printDataType (d)
        class (dataType(4,*)), intent(in) :: d

        if (allocated (d%data)) then
            print *, 'data allocated'
        else
            print *, 'data not allocated'
        end if
    end subroutine
end module

program ftpbnd510a3
use m
    type (node(20,4)), parameter :: default_node = node(20,4) (1, null())

    type (dataType(4,20)), parameter :: default_data = dataType(4,20)(null())

    call default_node%print
    call default_data%print
end
