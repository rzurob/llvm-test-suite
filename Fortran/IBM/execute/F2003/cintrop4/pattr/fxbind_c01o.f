! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c01o bind_c01i
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c01o.f
!* TEST CASE TITLE              : BIND(C) attribute
!*
!* PROGRAMMER                   : Yubin Liao
!* DATE                         : Sep. 1, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: entry BINC(C) attribute with 
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
       
   
