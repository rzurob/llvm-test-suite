! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg010a4.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg010a4.f
! %VERIFY: fArg010a4.out:fArg010a4.vf
! %STDIN:
! %STDOUT: fArg010a4.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute; pointer
!*                               component may be deallocated; will affect the
!*                               actual arg)
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
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: data (:) => null()
    end type

    contains

    subroutine abc (b)
        type (base(4)), value :: b

        if (associated (b%data)) then
            print *, 'deallocating data'

            deallocate (b%data)
        end if

        if (associated (b%data)) error stop 10_4
    end subroutine
end module

program fArg010a4
use m
    type (base(4)) :: b1

    allocate (b1%data(10))

    b1%data = 10

    call abc (b1)

    !! after this call the memory pointed to by b1%data are freed; but the
    !caller still has a copy of descrptor that tells him otherwise.  Reference
    !to b1%data from this point will result in garbled data at the best.

    !! the following 2 calls just test the descriptor's values
    if (.not. associated (b1%data)) error stop 1_4

    if (size (b1%data) /= 10) error stop 2_4
end
