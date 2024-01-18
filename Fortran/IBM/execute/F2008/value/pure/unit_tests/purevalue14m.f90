!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue14m.f
!*  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
!*  PROGRAMMER      : Gaby Baghdadi
!*  DATE            : 2010-12-01
!*  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - defines pure procedures used by purevalue14.f
!234567890123456789012345678901234567890123456789012345678901234567890123456789

pure integer function func1 (a)
    integer, value :: a
    func1 = a
    a = 100
end function

pure subroutine sub1 (a)
    integer, value :: a
    a = 200
end subroutine

module m
    interface 
        pure integer function func2 (a,b)
            integer, intent(in) :: a
            integer, value :: b
        end function
    
        pure subroutine sub2 (a,b)
            integer, intent(in) :: a
            integer, value :: b
        end subroutine
    end interface

contains
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
end module
    
pure integer function func2 (a,b)
    integer, intent(in) :: a
    integer, value :: b
    func2 = a + b
end function

pure subroutine sub2 (a,b)
    integer, intent(in) :: a
    integer, value :: b
    b = 200
end subroutine
