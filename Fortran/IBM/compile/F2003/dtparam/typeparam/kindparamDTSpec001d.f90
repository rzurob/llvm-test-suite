!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/12/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Use variables in DT entities'
!                               declarations. Use lenght type parameters in
!                               places of kind type-param for DT components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type base (k)
        integer, kind :: k = 8

        real(k) r
    end type

    integer :: n = 4

    type (base(n)) b1                   !<-- illegal
    class (base(k=:)), pointer :: b2    !<-- illegal

    type base1 (k, l)
        integer, kind :: k
        integer, len :: l

        class (base1(l,l)), pointer :: p1   !<-- illegal
!        class (base1(:,:)), pointer :: p2   !<-- illegal
!        procedure(type(base(n))), nopass, pointer :: p3   !<-- illegal
    end type

    type base2 (k, l)
        integer, kind :: k
        integer, len :: l

!        class (base1(l,l)), pointer :: p1   !<-- illegal
        class (base1(:,:)), pointer :: p2   !<-- illegal
!        procedure(type(base(n))), nopass, pointer :: p3   !<-- illegal
    end type

    type base3 (k, l)
        integer, kind :: k
        integer, len :: l

!        class (base1(l,l)), pointer :: p1   !<-- illegal
!        class (base1(:,:)), pointer :: p2   !<-- illegal
        procedure(type(base(n))), nopass, pointer :: p3   !<-- illegal
    end type

    end
