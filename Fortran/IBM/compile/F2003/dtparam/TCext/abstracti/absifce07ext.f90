! GM DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/abstracti/unit_tests/diag/absifce07.f

!***********************************************************************
!* =====================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : absifce07ext
!*
!*  PROGRAMMER                 : Glen Mateer (derived from absifce07
!*                               by James Ren)
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf90/95)
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The abstract interface name can not
!*  be referenced in type bound procedure.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
        contains
        procedure :: print => printBase
    end type

    abstract interface
        subroutine printBase (b, iounit)
        import
            class (base(4)), intent(in) :: b
            integer(selected_int_kind(1)), intent(in) :: iounit
        end subroutine
    end interface
end module

end
