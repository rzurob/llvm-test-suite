! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Sep. 1, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: module entry BINC(C) attribute with
!*                                array of different data types as
!*                                arguments. C calls Fortran.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
contains

       subroutine sextsub_arr1(sa, sb)
         integer sa(3,2,1), a(3,2,1)
         real    sb(3,2,1), b(3,2,1)
         sa = 33
         sb = 33.4
       entry extsub_arr1(a, b) BIND(C)
         a = 3
         b = 3.4
       end subroutine sextsub_arr1


       subroutine sextsub_arr2(sc, sl)
         character*1 sc(3,2,1), c(3,2,1)
         logical*1 sl(3,2,1), l(3, 2, 1)
         sc = 'a'
         sl = .false.
       entry extsub_arr2(c, l) bind(c)
         c = 'd'
         l = .true.
       end subroutine sextsub_arr2


       subroutine sextsub_arr3(sx)
         complex sx(3,2,1), x(3,2,1)
         sx = (10.0, 30.0)
       entry extsub_arr3(x) bind(c)
         x = (1.0, 3.0)
       end subroutine sextsub_arr3

 end module m