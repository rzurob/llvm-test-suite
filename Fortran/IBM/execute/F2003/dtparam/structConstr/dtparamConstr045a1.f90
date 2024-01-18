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
!*  DATE                       : 08/02/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               A function results as selector in a select type
!                               construct; structure constructor in allocate
!                               statement as a source-expr; the function result
!                               is a poly pointer array.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) :: x(dim)
    end type

    type, extends(point) ::colorPoint
        integer(2) :: color = 10
    end type

    contains

    function genColorPoint8 (d1, c)
        real(8), intent(in) :: d1(:,:)
        integer, intent(in) :: c(size(d1,2))

        class(point(8,size(d1,1))), pointer :: genColorPoint8(:)

        allocate (colorPoint(8,size(d1,1)):: genColorPoint8(size(d1,2)))

        do i = 1, size(d1,2)
            genColorPoint8(i)%x = d1(:,i)
        end do

        select type (genColorPoint8)
            type is (colorPoint(8,*))
                genColorPoint8%color = c

            class default
                stop 10
        end select
    end function
end module

program dtparamConstr045a1
use m
    double precision d1(10,20)

    logical(4), external :: precision_r8

    do i = 1, 20
        d1(:,i) = (/(sin((10*j+i)*1.0d0), j=1,10)/)
    end do

    select type (x => genColorPoint8 (d1, (/(i, i=1,20)/)))
        class is (colorPoint(8,*))
            if (x%dim /= 10) error stop 2_4

            do i = 1, 20
                do j = 1, 10
                    if (.not. precision_r8(x(i)%x(j), sin((j*10+i)*1.0d0))) &
                            error stop 3_4
                end do

                if (x(i)%color /= i) error stop 4_4
            end do

        class default
            error stop 1_4

    end select
end
