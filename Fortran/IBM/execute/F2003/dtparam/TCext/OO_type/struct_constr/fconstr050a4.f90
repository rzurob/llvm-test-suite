! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr050a4.f
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
!*  DATE                       : 02/22/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : structure constructor (rank two allocatable
!                               array component in structure constructor)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), allocatable :: data(:)
    end type

    type container(k2,n2)    ! (4,20)
        integer, kind                      :: k2
        integer, len                       :: n2
        type(dataType(k2,n2)), allocatable :: value(:,:)
    end type
end module

program fconstr050a4
use m
    type, extends(dataType) :: mainData(k3)    ! (4,20,4)
        integer, kind :: k3
        integer(k3)      id
    end type

    class (*), allocatable :: x1(:), x2(:)

    class (dataType(4,20)), allocatable :: d1(:,:)

    allocate (x1(2), source=(/10, 20/))
    allocate (x2(0:2), source=(/1.5, 3.2, 2.4/))

    allocate (d1(-1:0,0:1), source=reshape((/mainData(4,20,4)(x1, 1), mainData(4,20,4)(x1, 2), &
                   mainData(4,20,4)(x2,3), mainData(4,20,4)(x2, 4)/), (/2,2/)))


    call associate_replacer (container(4,20) (d1))

    contains

!    associate (x => container(4,20) (d1))
    subroutine associate_replacer (x)
        type(container(4,*)), intent(in) :: x

        if (.not. allocated (x%value)) error stop 1_4

        if (any(lbound(x%value) /= (/-1,0/))) error stop 2_4
        if (any(ubound(x%value) /= (/0, 1/))) error stop 3_4

        do i = -1, 0
            do j = 0, 1
                print *, '(', i,j, ')', 'bounds:', lbound(x%value(i,j)%data), &
                                        ubound(x%value(i,j)%data)

                select type (y => x%value(i,j)%data)
                    type is (integer)
                        print *, y
                    type is (real)
                        write (*, '(3(f10.2))') y
                    class default
                        error stop 10_4
                end select
            end do
        end do
    end subroutine
end
