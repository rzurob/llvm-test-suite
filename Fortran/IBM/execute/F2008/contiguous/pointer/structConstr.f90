! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-22
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - CONTIGUOUS array pointer as data-target
!*                                 in structure-constructor
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

module mod

    type A
        integer, contiguous, pointer :: x(:)
    end type

    type B
        integer, contiguous, pointer :: y(:)
    end type

end module

program main

    use mod

    type(A), allocatable :: dT
    type(B) :: oT

    integer, pointer :: ptr(:)

    allocate(ptr(6), source=[3,6,9,12,15,18])

    allocate(dT, source=A(ptr))

    oT = B(dT%x)

    if ( .not. all(oT%y .eq. [3,6,9,12,15,18] )) stop 21

end
