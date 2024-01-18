!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with literal as argument
!*                               while used as array element in interlanguage
!*                               calls with C.
!* ===================================================================

module mbnd2

     interface
        subroutine sub1(arg) bind(c)
            use  ISO_C_BINDING
            character(kind=C_CHAR) :: arg(5)
        end subroutine
     end interface

end module mbnd2

module mbnd22

     interface
        subroutine sub2(arg) bind(c)
            use  ISO_C_BINDING
            character(kind=C_CHAR) :: arg(5)
        end subroutine
     end interface

     procedure(sub2), pointer :: procptr

end module mbnd22

program mxminvalBindC1

    use  ISO_C_BINDING

    use mbnd2
    use mbnd22

    procptr => sub1

    call procptr((/maxval(reshape((/"df","fg", "we", "qs", "al", "ui"/), (/2,3/)))//C_NULL_CHAR, minval(reshape((/"df","fg", "we", "qs", "al", "ui"/), (/2,3/)))//C_NULL_CHAR, minval((/"qs", "al", "ui"/), dim=1)//C_NULL_CHAR, maxval(reshape((/"df","fg", "we", "qs", "al", "ui"/), (/2,3/)), mask=.true.)//C_NULL_CHAR, maxval((/"qs", "al", "ui"/), dim=1, mask=.true.)//C_NULL_CHAR/))

end program mxminvalBindC1
