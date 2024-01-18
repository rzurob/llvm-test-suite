! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/17/2005
!*
!*  DESCRIPTION                : C631, unlimited poly type can not appear in
!                               source-expr for data not of unlimited poly)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc005d
    type seq1(k1)    ! (4)
        integer, kind :: k1
        sequence

        integer(k1)      i1
    end type

    class (*), pointer :: x

    type (seq1(4)), target :: s1
    type (seq1(4)), pointer :: s2

    s1%i1 = 10

    x => s1

    allocate (s2, source=x)     !<-- illegal

    print *, s2

    s2 = x                      !<-- illegal

end
