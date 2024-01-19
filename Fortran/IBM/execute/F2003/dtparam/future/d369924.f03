! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2009-09-23
!*
!*  DESCRIPTION                : miscellaneous (defect 369924)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type container(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        private
        class(*), pointer :: data(:,:) => null()
    end type

    interface container
        module procedure makeContainerData2
    end interface

    contains

    type (container(4,20)) function makeContainerData2 (x2)
        class (*), intent(in) :: x2(:,:)

        allocate (makeContainerData2%data (size(x2,1), size(x2,2)), source=x2)
    end function
end module

module m1
use m
    type, extends(container) :: namedContainer(k2,n2)    ! (4,20,1,20)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: name
    end type

end module

program fconstr053a
use m1
    character (5), dimension (3,2) :: c1

    associate (x => namedContainer(4,20,1,20)(name = 'test 2', &
                container = container(c1)))

    end associate
end
