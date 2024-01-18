!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/26/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 325829)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A
        character(:), allocatable :: str
    end type

    type (A) a1, a2

    a1 = A ('test')

    a2 = A (genStr(a1, ' again') // '.')

    a1 = A(genStrWithFixedLen(a1, ' the last time') // '.')

    print *, a2%str
    print *, a1%str

    contains

    function genStr (a1, c)
        type(A), intent(in) :: a1
        character(*), intent(in) :: c

        character(:), allocatable :: genStr

        genStr = a1%str // c
    end function

    function genStrWithFixedLen (a1, c)
        type(A), intent(in) :: a1
        character(*), intent(in) :: c

        character(20), allocatable :: genStrWithFixedLen

        genStrWithFixedLen = a1%str // c
    end function
    end
