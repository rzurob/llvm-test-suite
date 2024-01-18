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
!*                               calls with C. 
!* ===================================================================

module modbnd2

     interface
        subroutine sub1(arg) bind(c)
            use  ISO_C_BINDING
            character(kind=C_CHAR) :: arg(5)
        end subroutine
     end interface

end module modbnd2

module modbnd22

     interface
        subroutine sub2(arg) bind(c)
            use  ISO_C_BINDING
            character(kind=C_CHAR) :: arg(5)
        end subroutine
     end interface

     procedure(sub2), pointer :: procptr

end module modbnd22

program mxminVarBindC3 

    use  ISO_C_BINDING

    use modbnd2
    use modbnd22

    character*2 tst1, tst2, tst3, tst4, tst5
    tst1 = "dd"
    tst2 = "xx"
    tst3 = "aa"
    tst4 = "ff"
    tst5 = "hh"

    procptr => sub1

    call procptr((/max(tst3,tst4)//C_NULL_CHAR, min(tst1, tst3, "zz")//C_NULL_CHAR, max(tst2, "d")//C_NULL_CHAR, min(tst1, "aa")//C_NULL_CHAR, tst5//C_NULL_CHAR/))

end program mxminVarBindC3 
