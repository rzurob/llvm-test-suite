! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrNotLog.f
! opt variations: -qnok -qnol -qnodeferredlp

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

    type A(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), pointer :: u1(:)
    end type

    type, extends(A) :: B    ! (4,20)
    end type

    type(B(4,20)), target :: b1
    type(A(4,:)), pointer :: ptr(:)

    allocate(A(4,20) :: ptr(2))

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

    b1 = B(4,20)(ptr)

    select type (x=>b1%u1)
        type is (A(4,*))
            x(1)%u1(size(ptr):) => x(2)%u1(5:2:-1)

            if (.not. associated(x(1)%u1 ,x(2)%u1(5:2:-1))) stop 3
            if ( lbound(x(1)%u1,1) /= 2 ) stop 4
            if ( ubound(x(1)%u1,1) /= 5 ) stop 5

            select type (y=>x(1)%u1)
                type is (logical)
                    if ( any( .not. y  .neqv.  &
                      (/.false., .true., .false., .true./))) stop 7
                class default
                    stop 9
            end select
        class default
            stop 10
    end select

end program


