! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/others/dataPtrFuncnamebound1.f
! opt variations: -ql

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
!* - data-pointer is component of DT, rank 2
!* - on the left size of =>, lb of data-ptr is function call making reference
!*     to the data-ptr itself
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: p(:,:)
    end type

    contains
        integer function ent(a)
            type(base(4)), intent(in) :: a
            ent = size(a%p)
        end function
end module

    program main
        use m

        type(base(4)) :: b2
        integer, target :: tar(4,5)
        type(base(4)) :: b2p

        tar = reshape ((/(i, i=1,20)/), (/ 4,5/))

        allocate(b2p%p(4,5))

        b2%p(size(b2p%p):, ent(b2p):)  => tar(::2,::2)

        if ( .not. associated(b2%p, tar(::2,::2))) error stop 10
        if ( any ( lbound(b2%p) .ne. (/20,20/) )) error stop 11
        if ( any ( ubound(b2%p) .ne. (/21,22 /) )) error stop 12
        print *, b2%p
    end program
