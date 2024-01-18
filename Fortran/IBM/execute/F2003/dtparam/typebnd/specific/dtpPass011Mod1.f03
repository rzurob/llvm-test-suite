! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/08/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               module used in dtpPass011 program
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        integer ids(n)

        contains

        procedure :: equal => compareB1B2
    end type

    contains

    function compareB1B2 (b1, b2)
        class(base(*)), intent(in) :: b1, b2

        logical :: compareB1B2 (max(b1%n, b2%n))

        compareB1B2 (:min(b1%n, b2%n)) = b1%ids(:min(b1%n, b2%n)) == &
            b2%ids (:min(b1%n, b2%n))

        compareB1B2 (min(b1%n, b2%n)+1:max(b1%n, b2%n)) = .false.
    end function
end module

