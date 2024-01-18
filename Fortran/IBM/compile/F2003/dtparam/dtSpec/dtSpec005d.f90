!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: C480: Each keyword shall specify a type
!                               parameter name; specify a wrong name.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtSpec005d
    class (*), pointer :: x

    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    !! the following 3 statements are all wrong in specifying type-param name
    type (base(kk=4, n = 100)) b1
    class (base(8, l = 20)), allocatable :: b2

    allocate (base(n1=20, k=8) :: x)
end
