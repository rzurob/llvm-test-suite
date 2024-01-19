! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc006a9d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (select type construct syntax error
!                               should be diagnosed)
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
    type, abstract :: base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(base) :: child(k2,k3,n2)    ! (4,20,4,1,21)
        integer, kind             :: k2,k3
        integer, len              :: n2
        integer(k2)                  id
        character(kind=k3,len=n2) :: name
    end type

    contains

    subroutine printX (x)
        class (*), allocatable, intent(in) :: x(:)

        if (.not. allocated(x)) error stop 1_4

        print *, 'bounds:', lbound(x), ubound(x)

        select type (x)
            type is (child(4,*,4,1,*))
                do i = lbound(x,1), ubound(x,1)
                    write (*, '(i5,a,a)', advance='no') x(i)%id, ' ', x(i)%name
                end do
            class default
                error stop 2_4
        !end select !<-- diagnose this
    end subroutine
end module

program falloc006a9d
end
