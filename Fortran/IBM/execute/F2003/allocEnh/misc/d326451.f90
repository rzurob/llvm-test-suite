! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/16/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 326451, functional case)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    integer function xyz1(i)
        write(*, *, iostat=xyz1) 10*i
    end function

    integer function xyz2(i)
        xyz2 = abc(i)

        contains

        integer function abc(i)
            write(*, *, iostat=xyz2) 10*i
            write(*, *, iostat=abc) 11*i

            abc = abc + xyz2
        end function
  end function
end module

use m

    i = xyz1(10)
    print *, i

    i = xyz2(11)

    print *, i
end

