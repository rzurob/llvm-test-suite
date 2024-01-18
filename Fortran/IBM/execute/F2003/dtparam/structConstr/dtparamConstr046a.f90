! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/10/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               pointer component in structure constructor
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A (n)
        integer, len :: n

        character(n), pointer :: name
    end type

    type (A(:)), allocatable :: a1

    character(:), allocatable, target :: c

    c = 'xlftest abc'

    a1 = A(11)(c)

    if (.not. associated(a1%name, c)) error stop 1

    if (a1%name /= 'xlftest abc') error stop 2

    print *, a1%name
    end
