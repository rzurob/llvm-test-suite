! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass002d.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/12/2005
!*
!*  DESCRIPTION                : CLASS keyword (unlimited poly entities in the
!                               array constructor)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fclass002d
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 10
    end type

    integer*4, target :: i = 1
    type(base(4)), target :: b1

    class(*), pointer :: x, x1

    x => i
    x1 => b1

    print *, (/x, x1/)      !<-- illegal

end
