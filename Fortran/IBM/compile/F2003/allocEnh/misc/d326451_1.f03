! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/16/2006
!*
!*  DESCRIPTION                : miscellanous (defect 326451, diagnostic case)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    integer function xyz(i)
        xyz = 10*i
    end function
end module

use m

    write (*,*, iostat=xyz) xyz(10)
end
