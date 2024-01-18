! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/allocEnh/misc/d328094.f
! opt variations: -qnol

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/15/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 328094)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)         x,y
    end type

    abstract interface
        elemental function unitPoint (p)
            import
            class(point(*,4)), intent(in) :: p

            type(point(20,4)) unitPoint
        end function
    end interface

    procedure(unitPoint), pointer :: gen

    contains

    subroutine Query
        print *, associated(gen)
    end subroutine
end module

!use m
!    gen => null()

!    call Query
end

