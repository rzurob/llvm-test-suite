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
!* DESCRIPTION                  : Test: BINC(C) attribute with
!*                                array of different data types
!*                                Using external subroutine, entry.
!*                                Both subroutine and entry have
!*                                bind(c) attribute.
!*                                C calls Fortran.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890



       subroutine sextsub_arr1(sa, sb) bind(c)
         integer sa(3,2,1), a(3,2,1)
         real    sb(3,2,1), b(3,2,1)
         sa = 33
         sb = 33.4
         return
       entry extsub_arr1(a, b) BIND(C)
         a = 3
         b = 3.4
       end subroutine sextsub_arr1

       subroutine sextsub_arr2(sc, sl) bind(c)
         character*1 sc(3,2,1), c(3,2,1)
         logical*1 sl(3,2,1), l(3,2,1)
         sc = 'a'
         sl = .false.
         return
       entry extsub_arr2(c, l) bind(c)
         c = 'd'
         l = .true.
       end subroutine sextsub_arr2

       subroutine sextsub_arr3(sx) bind(c)
         complex sx(3,2,1), x(3,2,1)
         sx = (10.0, 30.0)
         return
       entry extsub_arr3(x) bind(c)
         x = (1.0, 3.0)
       end subroutine sextsub_arr3


