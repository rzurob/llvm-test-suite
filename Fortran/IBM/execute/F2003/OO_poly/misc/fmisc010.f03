! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 277245; problem #1:
!                               declaration of allocatable array as function
!                               return result in explicit interface)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    interface
        integer(4) function t1 (i)
            allocatable t1(:)
            integer(4), intent(in) :: i
        end function

        function t2()
            real(4), allocatable :: t2(:)
        end function
    end interface
end module


module m1
    type base
        double precision, allocatable :: d1(:)
    end type

    interface
        function createBaseArray (n, d)
            import base
            class (base), allocatable :: createBaseArray(:)
            integer(4), intent(in) :: n
            real(8), intent(in) :: d
        end function
    end interface
end module

program fmisc010
    interface
        function t3(r, i, j)
            complex(4), allocatable :: t3(:,:)
            integer(4), intent(in) :: i, j
            real(4), intent(in) :: r
        end function

        function t4 (a, b)
            character(10), allocatable :: t4(:,:)
            real(4), intent(inout) :: a
            logical(8), intent(in) :: b(:)
        end function
    end interface
end