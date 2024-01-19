!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with variable as argument
!*                               while used as array element in interlanguage
!*                               calls with C.
!* ===================================================================

module mbnd1

     interface
        subroutine sub1(arg) bind(c)
            use  ISO_C_BINDING
            character(kind=C_CHAR) :: arg(5)
        end subroutine
     end interface

end module mbnd1

module mbnd11

     interface
        subroutine sub2(arg) bind(c)
            use  ISO_C_BINDING
            character(kind=C_CHAR) :: arg(5)
        end subroutine
     end interface

     procedure(sub2), pointer :: procptr

end module mbnd11

program mxminvalVarBindC1

    use  ISO_C_BINDING

    use mbnd1
    use mbnd11

    character(kind=C_CHAR, len=3) :: cc(5)

    character*2 tst1(2,3), tst2(3)
    tst1 = reshape((/"df","fg", "we", "qs", "al", "ui"/), (/2,3/))
    tst2 = (/"qs", "al", "ui"/)

    procptr => sub1

    call procptr((/maxval(tst1)//C_NULL_CHAR, minval(tst1)//C_NULL_CHAR, minval(tst2, dim=1)//C_NULL_CHAR, maxval(tst1, mask=.true.)//C_NULL_CHAR, maxval(tst2, dim=1, mask=.true.)//C_NULL_CHAR/))

end program mxminvalVarBindC1
