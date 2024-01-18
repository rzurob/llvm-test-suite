!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/25/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly function results (use the RESULT keyword
!                               in function definition; test the rank-two array)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8), allocatable :: ids(:)
    end type

    type, extends(base) :: child
        character(20), allocatable :: names(:)
    end type

    interface makeData
        function makeDataArray2 (ids, names, shape) result (array)
        import base
            integer(8), intent(in) :: ids(:)
            character(*), intent(in), optional :: names(size(ids))
            integer, intent(in) :: shape(2)

            class(base), pointer :: array(:,:)
        end function
    end interface
end module


function makeDataArray2 (ids, names, shape) result (array)
use m, only: base, child
    integer(8), intent(in) :: ids(:)
    character(*), intent(in), optional :: names(size(ids))
    integer, intent(in) :: shape(2)

    class(base), pointer :: array(:,:)

    if (present(names)) then
        allocate (array(shape(1),shape(2)), source=child(ids, names))
    else
        allocate (array(shape(1),shape(2)), source=base(ids))
    end if
end function


program ffuncRet008a
use m
    select type (x => makeData((/1_8, 3_8, 5_8/), shape=(/5,6/)))
        class default
            if (any(shape(x) /= (/5,6/))) error stop 1_4

            select type (y => x)
                type is (base)
                    do i = 1, 5
                        do j = 1, 6
                            if (.not. allocated(y(i,j)%ids)) error stop 2_4

                            if (any (y(i,j)%ids /= (/1,3,5/))) error stop 3_4
                        end do
                    end do
                class default
                    error stop 5_4
            end select
    end select

    select type (x => makeData((/10_8, 20_8, 30_8, 40_8/), shape = (/10, 20/), &
                        names=(/'test1', 'test2', 'test3', 'test4'/)))
        class default
            if (any(shape(x) /= (/10, 20/))) error stop 6_4

            select type (y => x)
                type is (child)
                    do i = 1, 10
                        do j = 1, 20
                            if ((.not. allocated(y(i,j)%ids)) .or. (.not. &
                                    allocated(y(i,j)%names))) error stop 7_4

                            if (any (y(i,j)%ids /= (/10,20,30,40/))) error stop 8_4
                            if (any (y(i,j)%names /= (/'test1', 'test2', &
                                        'test3', 'test4'/))) error stop 9_4
                        end do
                    end do
                class default
                    error stop 10_4
            end select
    end select
end
