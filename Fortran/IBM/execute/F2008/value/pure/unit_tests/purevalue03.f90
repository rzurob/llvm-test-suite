!******************************************************************************
!*  ===========================================================================
!*
!*  DATE            : 2010-12-01
!*
!*  DESCRIPTION
!*  - several pure procedures with mixed # of dummy arguments with value
!*    attribute preceded and succeeded by intent(in|out|inout)
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main
    integer :: sa=1, sb=2, sc, sr

    sc = func1(sa)
    if (sc /= sa) error stop 2

    sc = func2(sa,sb)
    if (sc /= (sa+sb)) error stop 3
    if (sb /= 2) error stop 4

    sr = func3(sa,sb,sc,sa,sb,sc)
    if (sr /= 12) error stop 5
    if (any([sa,sb,sc] /= [1,2,3])) error stop 6

    call sub1(sa)
    if (sa /= 1) error stop 7

    call sub2(sa,sb)
    if (any([sa,sb] /= [1,2])) error stop 8

    call sub3(sa,sb,sc,sa,sb,sc)
    if (any([sa,sb,sc] /= [-10,2,1])) error stop 9

contains
    pure integer function func1 (a)
        integer, value :: a
        func1 = a
        a = 0
    end function

    pure integer function func2 (a,b)
        integer, intent(in) :: a
        integer, value :: b
        func2 = a + b
    end function

    pure function func3 (a,b,c,d,e,f)
        integer, value :: a
        integer, intent(in) :: b
        integer, value :: c
        integer, intent(in) :: d
        integer, value :: e
        integer, intent(in) :: f
        func3 = a + b + c + d + e + f
        a = 0; c = 0; e =0
    end function

    pure subroutine sub1 (a)
        integer, value :: a
        a = 100
    end subroutine

    pure subroutine sub2 (a,b)
        integer, intent(in) :: a
        integer, value :: b
        b = 200
    end subroutine

    pure subroutine sub3 (a,b,c,d,e,f)
        integer, value :: a
        integer, intent(in) :: b
        integer, value :: c
        integer, intent(out) :: d
        integer, value :: e
        integer, intent(inout) :: f
        d = a - b - c - d - e - f
        f = a + b + c + d + e + f
        a = 0; c = 0; e = 0
    end subroutine

end
