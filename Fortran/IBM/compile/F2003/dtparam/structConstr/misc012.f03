! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/20/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 317599)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    interface
        function bad (n) result (x)
            procedure(real) x
        end function
    end interface
end module

program misc012
    interface
        function bad (n)
            procedure(real) bad
        end function
    end interface
end
