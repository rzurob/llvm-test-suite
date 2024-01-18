!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue13.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - pass vector data types as dummy args with value attributes to pure
!*    procedures
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main
    implicit none
    external precision_r4
    logical precision_r4
    vector(integer(1)) :: vi1
    vector(integer(2)) :: vi2
    vector(integer(4)) :: vi4
    vector(integer(8)) :: vi8
    integer(1) :: si1(16) = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
    integer(2) :: si2(8) = [21,22,23,24,25,26,27,28]
    integer(4) :: si4(4) = [41,42,43,44]
    integer(8) :: si8(2) = [81,82]
    equivalence(vi1,si1)
    equivalence(vi2,si2)
    equivalence(vi4,si4)
    equivalence(vi8,si8)

    vector(unsigned(1)) :: vu1
    vector(unsigned(2)) :: vu2
    vector(unsigned(4)) :: vu4
    vector(unsigned(8)) :: vu8
    equivalence(vu1,si1)
    equivalence(vu2,si2)
    equivalence(vu4,si4)
    equivalence(vu8,si8)

    vector(real(4)) :: vr4
    vector(real(8)) :: vr8
    real(4) :: sr4(4) = [41.42,43.44,45.46,47.48]
    real(8) :: sr8(2) = [81.82,83.84]
    equivalence(vr4,sr4)
    equivalence(vr8,sr8)

    vector(pixel) :: vp
    integer(2) :: sp = b'1011011010111011'
    equivalence(vp,sp)

    call foo(vi1,vi2,vi4,vi8,vu1,vu2,vu4,vu8,vr4,vr8,vp)

contains
    pure subroutine foo (vi1,vi2,vi4,vi8,vu1,vu2,vu4,vu8,vr4,vr8,vp)
            vector(integer(1)), value :: vi1
            vector(integer(2)), value :: vi2
            vector(integer(4)), value :: vi4
            vector(integer(8)), value :: vi8
            vector(unsigned(1)), value :: vu1
            vector(unsigned(2)), value :: vu2
            vector(unsigned(4)), value :: vu4
            vector(unsigned(8)), value :: vu8
            vector(real(4)), value :: vr4
            vector(real(8)), value :: vr8
            vector(pixel), value :: vp

            integer(1) :: i1(16)
            integer(2) :: i2(8)
            integer(4) :: i4(4)
            integer(8) :: i8(2)
            real(4) :: r4(4)
            real(8) :: r8(2)
            integer(2) :: p2

        integer :: i,tmp
        i1 = [(vec_extract(vi1,i),i=0,15)]
        i2 = [(vec_extract(vi2,i),i=0,7)]
        i4 = [(vec_extract(vi4,i),i=0,3)]
        i8 = [(vec_extract(vi8,i),i=0,1)]
        if (any(i1 .ne. [(i,i=1,16)])) tmp = tmp/0
        if (any(i2 .ne. [(i,i=21,28)])) tmp = tmp/0
        if (any(i4 .ne. [(i,i=41,44)])) tmp = tmp/0
        if (any(i8 .ne. [(i,i=81,82)])) tmp = tmp/0

        i1 = transfer(vu1,i1)
        i2 = transfer(vu2,i2)
        i4 = transfer(vu4,i4)
        i8 = transfer(vu8,i8)
        if (any(i1 .ne. [(i,i=1,16)])) tmp = tmp/0
        if (any(i2 .ne. [(i,i=21,28)])) tmp = tmp/0
        if (any(i4 .ne. [(i,i=41,44)])) tmp = tmp/0
        if (any(i8 .ne. [(i,i=81,82)])) tmp = tmp/0

        r4 = [(vec_extract(vr4,i),i=0,3)]
        r8 = [(vec_extract(vr8,i),i=0,1)]
        if (any(r4 .ne. [41.42,43.44,45.46,47.48])) tmp = tmp/0
        if (any(r8 .ne. [81.82,83.84])) tmp = tmp/0

        p2 = transfer(vp,p2)
        if (p2 /= b'1011011010111011') tmp = tmp/0
    end subroutine
end
