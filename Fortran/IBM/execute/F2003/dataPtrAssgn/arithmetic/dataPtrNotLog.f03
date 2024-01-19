!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-ptr is of type class(*), a component of nested derived-type
!* - ptr assignment contains pointer self reference
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

    type A
        class(*), pointer :: u1(:)
    end type

    type, extends(A) :: B
    end type

    type(B), target :: b1
    type(A), pointer :: ptr(:)

    allocate(ptr(2))

    do i = 1, 2
        allocate(ptr(i)%u1(5), source = (/ .true.,.true.,.false.,.true.,.false. /))
    enddo

    select type (x=>ptr(2)%u1)
        type is (logical)
            select type ( y => ptr(1)%u1)
                type is (logical)
                    x = cshift(y,1)
                class default
                    stop 1
            end select
        class default
           stop 2
    end select

    b1 = B(ptr)

    select type (x=>b1%u1)
        type is (A)
            x(1)%u1(size(ptr):) => x(2)%u1(5:2:-1)

            if (.not. associated(x(1)%u1 ,x(2)%u1(5:2:-1))) error stop 3
            if ( lbound(x(1)%u1,1) /= 2 ) error stop 4
            if ( ubound(x(1)%u1,1) /= 5 ) error stop 5

            select type (y=>x(1)%u1)
                type is (logical)
                    if ( any( .not. y  .neqv.  &
                      (/.false., .true., .false., .true./))) error stop 7
                class default
                    stop 9
            end select
        class default
            stop 10
    end select

end program


