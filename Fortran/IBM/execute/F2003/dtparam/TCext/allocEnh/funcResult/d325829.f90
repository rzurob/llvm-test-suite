! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/allocEnh/funcResult/d325829.f
! opt variations: -qck -qnok -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/26/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 325829)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A(k1)    ! (4)
        integer, kind :: k1
        character(:), allocatable :: str
    end type

    type (A(4)) a1, a2

    a1 = A(4) ('test')

    a2 = A(4) (genStr(a1, ' again') // '.')

    a1 = A(4)(genStrWithFixedLen(a1, ' the last time') // '.')

    print *, a2%str
    print *, a1%str

    contains

    function genStr (a1, c)
        type(A(4)), intent(in) :: a1
        character(*), intent(in) :: c

        character(:), allocatable :: genStr

        genStr = a1%str // c
    end function

    function genStrWithFixedLen (a1, c)
        type(A(4)), intent(in) :: a1
        character(*), intent(in) :: c

        character(20), allocatable :: genStrWithFixedLen

        genStrWithFixedLen = a1%str // c
    end function
    end
