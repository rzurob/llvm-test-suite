! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/misc/d272132.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/05/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 272132)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: x
    end type
end module

    use m

    interface base
        subroutine abc
        end subroutine
    end interface

    call base()

end

subroutine abc
    print *, 'abc'
end subroutine
