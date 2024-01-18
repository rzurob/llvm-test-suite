! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/allocEnh/misc/abstractInterface001d.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/15/2006
!*
!*  DESCRIPTION                : miscellaneous
!                               diagnostic about abstract interface
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id

        contains

        procedure :: printBase
    end type

    abstract interface
        subroutine printBase (b, iounit)
        import
            class (base(*,4)), intent(in) :: b
            integer(selected_int_kind(1)), intent(in) :: iounit
        end subroutine
    end interface
end module

end
