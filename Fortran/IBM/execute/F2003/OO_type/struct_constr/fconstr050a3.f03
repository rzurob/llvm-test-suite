! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (array section used as
!                               the data-source for nonpoly-allocatable
!                               component)
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
        integer(4) id
    end type

    type, extends(base) :: child
        character(10) :: name
    end type
end module

program fconstr050a3
use m
    type container
        type (base), allocatable :: data(:)
    end type

    class (base), allocatable :: b1(:,:)

    allocate (b1(0:1,-1:0), source=reshape ((/(child(i,'test_101'),i=1,4)/), &
                            (/2,2/)))

    associate (x => container (b1(:,0)))
        if (.not. allocated (x%data)) error stop 1_4

        if ((lbound(x%data,1) /= 1) .or. (ubound(x%data,1) /= 2)) error stop 2_4

        if (any (x%data%id /= (/3,4/))) error stop 3_4
    end associate
end