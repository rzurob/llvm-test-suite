!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 2/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with variable as argument 
!*                               while used as array element in interlanguage 
!*                               calls with C. Also test whether null is
!*                               appended or not. 
!* ===================================================================

module modbnd1

     interface
        subroutine sub1(arg) bind(c)
            use  ISO_C_BINDING
            character(kind=C_CHAR) :: arg(5)
        end subroutine
     end interface

end module modbnd1

module modbnd11

     interface
        subroutine sub2(arg) bind(c)
            use  ISO_C_BINDING
            character(kind=C_CHAR) :: arg(5)
        end subroutine
     end interface

     procedure(sub2), pointer :: procptr

end module modbnd11

program mxminArgBindC3 

    use  ISO_C_BINDING

    use modbnd1
    use modbnd11

    character(kind=C_CHAR, len=3) :: cc(5)

    character*2 tst1, tst2
    tst1 = "dd"
    tst2 = "xx"

    procptr => sub1

    call procptr((/max("aa","ff")//C_NULL_CHAR, min("dd", "aa", "zz")//C_NULL_CHAR, max("xx", "d")//C_NULL_CHAR, min(tst1, "aa")//C_NULL_CHAR, "hh"//C_NULL_CHAR/))

end program mxminArgBindC3 
