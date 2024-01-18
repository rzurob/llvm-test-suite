!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/04/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               Diagnostic: length type parameter must be
!                               assumed.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n = 100

        real data(n)

        procedure(genSum), pointer :: p
    end type

    interface
        real function genSum (b1)
        import
            class(base) b1  !<-- b1%n must be assumed
        end function
    end interface
end module

end
