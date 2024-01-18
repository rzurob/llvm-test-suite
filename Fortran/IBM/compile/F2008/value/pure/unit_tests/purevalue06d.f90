!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue06d.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - dummy data objects declared with neither intent(in) nor the valueattribute
!*  - test above for pure and elemental functions and subroutines, with dummy
!*    args of vector data types
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

pure subroutine psub (vi1,vi2,vi4,vi8,vu1,vu2,vu4,vu8,vr4,vr8,vp)
    vector(integer(1)) :: vi1
    vector(integer(2)) :: vi2
    vector(integer(4)) :: vi4
    vector(integer(8)) :: vi8
    vector(unsigned(1)) :: vu1
    vector(unsigned(2)) :: vu2
    vector(unsigned(4)) :: vu4
    vector(unsigned(8)) :: vu8
    vector(real(4)) :: vr4
    vector(real(8)) :: vr8
    vector(pixel) :: vp
end subroutine

integer pure function pfunc (vi1,vi2,vi4,vi8,vu1,vu2,vu4,vu8,vr4,vr8,vp)
    vector(integer(1)) :: vi1
    vector(integer(2)) :: vi2
    vector(integer(4)) :: vi4
    vector(integer(8)) :: vi8
    vector(unsigned(1)) :: vu1
    vector(unsigned(2)) :: vu2
    vector(unsigned(4)) :: vu4
    vector(unsigned(8)) :: vu8
    vector(real(4)) :: vr4
    vector(real(8)) :: vr8
    vector(pixel) :: vp
    pfunc = 0
end function

elemental subroutine esub (vi1,vi2,vi4,vi8,vu1,vu2,vu4,vu8,vr4,vr8,vp)
    vector(integer(1)) :: vi1
    vector(integer(2)) :: vi2
    vector(integer(4)) :: vi4
    vector(integer(8)) :: vi8
    vector(unsigned(1)) :: vu1
    vector(unsigned(2)) :: vu2
    vector(unsigned(4)) :: vu4
    vector(unsigned(8)) :: vu8
    vector(real(4)) :: vr4
    vector(real(8)) :: vr8
    vector(pixel) :: vp
end subroutine

real*16 elemental function efunc (vi1,vi2,vi4,vi8,vu1,vu2,vu4,vu8,vr4,vr8,vp)
    vector(integer(1)) :: vi1
    vector(integer(2)) :: vi2
    vector(integer(4)) :: vi4
    vector(integer(8)) :: vi8
    vector(unsigned(1)) :: vu1
    vector(unsigned(2)) :: vu2
    vector(unsigned(4)) :: vu4
    vector(unsigned(8)) :: vu8
    vector(real(4)) :: vr4
    vector(real(8)) :: vr8
    vector(pixel) :: vp
    efunc = 0.0_16
end function
