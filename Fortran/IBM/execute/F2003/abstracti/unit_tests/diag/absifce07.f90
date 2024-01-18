!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The abstract interface name can not
!*                               be referenced in type bound procedure.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base
        integer id
        contains
        procedure :: print => printBase
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

