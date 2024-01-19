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
    type base
        integer id

        contains

        procedure :: printBase
    end type

    abstract interface
        subroutine printBase (b, iounit)
        import
            class (base), intent(in) :: b
            integer(selected_int_kind(1)), intent(in) :: iounit
        end subroutine
    end interface
end module

end
