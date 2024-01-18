!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: C479: Omit the keyword but the preceding
!                               keyword is not omitted.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real :: data(n)
    end type

    type, extends(base) :: child (k)
        integer, kind :: k

        integer(k) :: ids(n)
    end type

    type (child(n=200, 8)) :: c1_m                  !<-- illegal
    class (child(n=:, 4)), allocatable :: c2_m(:)   !<-- illegal
    class (child(k=8, :)), pointer :: c3_m(:,:,:)   !<-- illegal
end module

program dtSpec004d
end
