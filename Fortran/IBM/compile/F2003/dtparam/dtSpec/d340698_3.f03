! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/10/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 340698, test case 3)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type x (n)
        integer, len :: n

        real data(n)
    end type

    class(X(:)), allocatable :: y

    print *, [X(*) :: ]   !<-- illegal
    print *, [X(:) :: ]   !<-- illegal
    end
