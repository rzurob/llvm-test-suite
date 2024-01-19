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
!* - ptr is of class(base), target is of class(child) with allocatable attribute
!* - ptr is of class(*), target is of class(base)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    integer :: count = 0

    type A
    end type

    type, extends(A) :: base
    end type

    type, extends(base) :: child
        character(:), allocatable :: ch
    end type
end module

   program main
        use m
    class(base), pointer :: p(:)
    class(child), target,  allocatable :: t(:)
    class(A), pointer :: q(:,:)

    allocate(t(100), source = (/ ( child('OO'), i=1,100 ) /))

    p(func(100):) => t(1:func(100))

    if ( .not. allocated(t) ) error stop 99

    if ( .not. associated(p) ) error stop 1
    if ( lbound(p,1) /= 50) error stop 3
    if ( ubound(p,1) /= 99) error stop 5

    q(func(10):func(20),1:lbound(p,1)/10) => p

    if ( .not. associated(q) ) error stop 11
    if ( any (lbound(q) .ne. (/50, 1/)) ) error stop 13
    if ( any (ubound(q) .ne. (/50, 5/)) ) error stop 15

    select type (q)
        type is (child)
            print *, (/ (q(func(30),i)%ch, i = 1,5) /)
        type is (base)
        stop 23
        class default
         stop 25
    end select

    contains
        function func(num)
        integer num

        count  = count + 1
        func = 50
        end function

   end program
