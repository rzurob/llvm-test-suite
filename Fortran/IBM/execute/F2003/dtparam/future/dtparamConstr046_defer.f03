! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/08/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Put the structure constructor as a selector in
!                               an associate construct; component is a pointer
!                               array of rank two.
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
        integer(2) color
    end type

    interface genPoint
        module procedure genPoint4
        procedure genColorPoint8
    end interface

    type base (k)
        integer, kind :: k

        class(point(k,:)), pointer :: point(:,:)
    end type

    contains

    function genPoint4 (r1, n1, n2, dim)
        real(4), intent(in) :: r1(n1*n2*dim)
        integer, intent(in) :: n1, n2, dim

        class (point(4,dim)), pointer :: genPoint4(:,:)

        allocate (genPoint4(n1,n2))

        do i = 1, n1
            do j = 1, n2
                genPoint4(i,j)%x = r1(((j-1)*n1+i-1)*dim+1:((j-1)*n1+i)*dim)
            end do
        end do
    end function

    function genColorPoint8 (d1, n1, n2, dim, c)
        real(8), intent(in) :: d1(n1*n2*dim)
        integer, intent(in) :: n1, n2, dim, c(n1*n2)

        class(point(8,:)), pointer :: genColorPoint8(:,:)

        type(colorPoint(8,dim)) local(n1*n2)

        local=(/((/(colorPoint(8,dim)(d1(((j-1)*n1+i-1)*dim+1:((j-1)*n1+i)*dim),&
            c((j-1)*n1+i)), i=1,n1)/), j=1,n2)/)

        allocate (genColorPoint8(n1,n2), source=reshape(local, (/n1, n2/)))
    end function
end module

program dtparamConstr046
use m
    real(4) r1(10*5*3)

    double precision d1(30*5*2)

    logical(4), external :: precision_r4, precision_r8

    r1 = (/(sin(i*1.0), i=1,size(r1))/)

    !! first test: point(4,3)
    associate (x => base(4)(genPoint(r1, 10, 5, 3)))
        if (.not. associated(x%point)) error stop 1_4

        if (.not. same_type_as (x%point, point(4,3)(x=1.0))) error stop 2_4

        if (any(shape(x%point) /= (/10,5/))) error stop 3_4

        icount = 1

        do j = 1, 5
            do i = 1, 10
                do k = 1, 3
                    if (.not. precision_r4(sin(icount*1.0), x%point(i,j)%x(k)))&
                        error stop 4_4

                    icount = icount + 1
                end do
            end do
        end do

!        deallocate (x%point)
    end associate

    d1 = log((/(i*1.0d0, i=1, size(d1))/))

    associate (x => base(8)(genPoint(d1, 30, 5, 2, (/(i, i=1,150)/))))
        if (.not. associated(x%point)) error stop 5_4

        if (.not. same_type_as(colorPoint(8,2)(0, 0),x%point)) error stop 6_4

        if (any(shape(x%point) /= (/30,5/))) error stop 7_4

        if (size(x%point(1,1)%x) /= 2) error stop 8_4

        icount = 1

        do j = 1, 5
            do i = 1, 30
                select type (y => x%point(i,j))
                    class is (colorPoint(8,*))
                        if (y%color /= (icount+1)/2) error stop 9_4

                        if (.not. precision_r8(log(icount*1.0d0), y%x(1))) &
                            error stop 10_4

                        if (.not. precision_r8(log((icount+1)*1.0d0), y%x(2))) &
                            error stop 11_4

                    class default
                        error stop 15_4
                end select

                icount = icount + 2
            end do
        end do

!        deallocate (x%point)
    end associate
end
