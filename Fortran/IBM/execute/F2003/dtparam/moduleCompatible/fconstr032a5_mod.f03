module m
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure :: print => printData
    end type

    type container(k2,n2)    ! (4,20)
        integer, kind                       :: k2
        integer, len                        :: n2
        integer(k2)                            id
        class(dataType(k2,n2)), allocatable :: data(:)
    end type

    contains

    subroutine printData (d)
        class (dataType(4,*)), intent(in) :: d

        print *, 'dataType'
    end subroutine
end module

module m1
use m, only : dataType
    type, extends(dataType) :: realData(k3)    ! (4,20,8)
        integer, kind :: k3
        real(k3)      :: data

        contains

        procedure, pass(d) :: print => printRealData
    end type

    contains

    subroutine printRealData (d)
        class(realData(4,*,8)), intent(in) :: d

        write (*, '(g10.3)') d%data
    end subroutine
end module
