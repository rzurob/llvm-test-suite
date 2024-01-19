! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc006a5.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (derived type as the type-spec in
!                               allocate statement for rank-one allocatable
!                               unlimited poly-array; involve select type)
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
        integer, kind        :: k1
        integer(k1), pointer :: id => null()
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,18)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'
    end type

    contains

    subroutine printX (x)
        class (*), allocatable, intent(in) :: x(:)

        if (.not. allocated(x)) error stop 1_4

        print *, 'bounds:', lbound(x,1), ubound(x, 1)

        select type (x)
            class is (base(4))
                call printBaseArray (x)
            class default
                error stop 2_4
        end select
    end subroutine


    subroutine printBaseArray (b1)
        class (base(4)), intent(in) :: b1(:)

        do i = 1, size (b1)
            if (associated (b1(i)%id)) then
                write (*, '(i5)', advance='no') b1(i)%id
            else
                write (*, '(a)', advance='no') 'id not associated'
            end if

            select type (x => b1(i))
                type is (base(4))
                    print *, ''
                type is (child(4,1,*))
                    print *, ', ', x%name
                class default
                    error stop 5_4
            end select
        end do
    end subroutine

    subroutine updateX (x, ids, names)
        class(*), intent(inout), allocatable :: x(:)
        integer, intent(in) :: ids(:)
        character(*), intent(in) :: names(:)

        if (size (ids) /= size(names)) error stop 10_4

        if (.not. allocated (x)) error stop 11_4

        if (size (x) /= size(ids)) error stop 12_4

        select type (x)
            class is (base(4))
                k = 1
                do i = lbound(x,1), ubound(x,1)
                    if (associated (x(i)%id)) then
                        x(i)%id = ids(k)
                    else
                        allocate (x(i)%id, source=ids(k))
                    end if

                    k = k + 1
                end do

                select type (x)
                    type is (child(4,1,*))
                        k = 1
                        do i = lbound(x,1), ubound(x,1)
                            x(i)%name = names(k)

                            k = k + 1
                        end do
                end select
            class default
                error stop 13_4
        end select
    end subroutine
end module


program falloc006a5
use m
    class (*), allocatable :: x1(:)

    allocate (child(4,1,18) :: x1(0:2))

    call printX (x1)

    call updateX (x1, (/1,2,3/), (/'xlftest 1', 'xlftest 2', 'xlftest 3'/))

    call printX (x1)

end
