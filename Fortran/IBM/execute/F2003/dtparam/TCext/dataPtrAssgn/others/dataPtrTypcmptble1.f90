! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/others/dataPtrTypcmptble1.f
! opt variations: -qck -qnok -ql

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrTypcmptble1.f
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

        type A(k1)    ! (4)
            integer, kind :: k1
    end type

    type, extends(A) :: base    ! (4)
    end type

    type, extends(base) :: child    ! (4)
        character(:), allocatable :: ch
    end type
   end module

program main
        use m
    class(base(4)), pointer :: p(:)
    class(child(4)), target,  allocatable :: t(:)
    class(A(4)), pointer :: q(:,:)

    allocate(t(100), source = (/( child(4)('OO'), i=1,100 ) /))

    p(func(100):) => t(1:func(100))

    if ( .not. allocated(t) ) stop 99

    if ( .not. associated(p) ) stop 1
    if ( lbound(p,1) /= 50) stop 3
    if ( ubound(p,1) /= 99) stop 5

    q(func(10):func(20),1:lbound(p,1)/10) => p

    if ( .not. associated(q) ) stop 11
    if ( any (lbound(q) .ne. (/50, 1/)) ) stop 13
    if ( any (ubound(q) .ne. (/50, 5/)) ) stop 15

    select type (q)
        type is (child(4))
            print *, (/ (q(func(30),i)%ch, i = 1,5) /)
        type is (base(4))
        stop 23
        class default
        stop 25
    end select

    contains
        function func(num)
        integer num, func

        count  = count + 1
        func = 50
        end function
end program
