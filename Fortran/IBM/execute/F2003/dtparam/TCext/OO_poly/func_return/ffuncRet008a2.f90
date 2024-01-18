! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet008a2.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/25/2005
!*
!*  DESCRIPTION                : poly-function results (result keyword use in
!                               the function definition; unlimited poly function
!                               return allocatable array)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    function makeArray (x, size) result (array)
        class(*), intent(in) :: x
        integer, intent(in) :: size

        class (*), allocatable :: array(:)

        allocate (array(size), source=x)
    end function
end module

program ffuncRet008a2
use m
    type base(k1,n1)    ! (1,20)
        integer, kind                          :: k1
        integer, len                           :: n1
        character(kind=k1,len=n1), allocatable :: name(:)
    end type

    call associate_replacer (makeArray (base(1,20)((/'abc', 'xyz', '123'/)), 10))
    contains

!    associate (x => makeArray (base(1,20)((/'abc', 'xyz', '123'/)), 10))
    subroutine associate_replacer(x)
        class(*), intent(in) :: x(:)
        if (size(x) /= 10) error stop 1_4

        select type (y => x)
            type is (base(1,*))
                do i = 1, 10
                    if (.not. allocated(y(i)%name)) error stop 2_4

                    if (any (y(i)%name /= (/'abc', 'xyz', '123'/))) error stop 3_4
                end do
            class default
                error stop 5_4
        end select
    end subroutine
end
