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
    type base
        integer(4) :: x
    end type
end module

    use m

    interface base
        subroutine abc
        end subroutine
    end interface

    call base

end

subroutine abc
    print *, 'abc'
end subroutine

