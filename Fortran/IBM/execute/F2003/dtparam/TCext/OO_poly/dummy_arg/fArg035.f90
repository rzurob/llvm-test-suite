! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg035.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/17/2005
!*
!*  DESCRIPTION                : argument association (for allocatable dummy-arg
!                               the actual-arg is allowed to have an allocatio
!                               status of unallocated)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), pointer :: data(:) => null()
    end type

    type, extends(base) :: child(k2)    ! (4,20,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name
    end type

    contains

    class(base(4,:)) function copyData (b)
        class (base(4,:)), allocatable, intent(in) :: b(:)
        allocatable copyData(:)

        if (allocated (b)) allocate (copyData(size(b)), source=b)
    end function
end module

program fArg035
use m
    class(base(4,:)), allocatable :: b1(:)

    if (allocated (copyData(null(b1)))) error stop 1_4

    if (allocated (copyData(copyData(null(b1))))) error stop 2_4

    if (allocated (copyData(copyData(copyData(null(b1)))))) error stop 3_4

    if (allocated (copyData(copyData(copyData(copyData(null(b1))))))) &
                error stop 4_4
end
