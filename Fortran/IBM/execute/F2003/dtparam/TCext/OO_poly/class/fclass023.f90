! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass023.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/03/2005
!*
!*  DESCRIPTION                : CLASS keyword (entry statement used in external
!                               subprogram)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    interface genData
        class(base(4)) function genPtr(i)
        import
            pointer genPtr
        end function

        class(base(4)) function genNULL ()
        import
            pointer genNULL
        end function
    end interface
end module

function genPtr(i) result (returnVal)
use m, only: base
    class(base(4)), pointer :: returnVal

    allocate (returnVal, source=base(4)(i))
    return

    entry genNull () result (returnVal)
    nullify (returnVal)

end function


program fclass023
use m
    class(base(4)), pointer :: b1, b2

    b1 => genData(10)
    b2 => genData()

    if ((.not. associated(b1)) .or. associated(b2)) error stop 1_4

    if (b1%id /= 10) error stop 2_4

end
