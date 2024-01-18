! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c01l bind_c01l
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
!* TEST CASE TITLE              : fxbind_c01l.f
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
!* DESCRIPTION                  : Test: BINC(C) attribute 
!*                                array of different data types  
!*                                Using external subroutine,interface.
!*                                C calls fortran. with binding lables
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


                   
       subroutine extsub_arr1(a, b) bind(c, name = "sub_arr1") 
         integer a(3,2,1)
         real    b(3,2,1)
         a = 3
         b = 3.4
       end subroutine extsub_arr1
       
       subroutine extsub_arr2(c, l) bind(c, name = "sub_arr2") 
         character*1 c(3,2,1)
         logical*1 l(3,2,1)
         c = 'd'
         l = .true.
       end subroutine extsub_arr2
       
       subroutine extsub_arr3(x) bind(c, name = "sub_arr3")
         complex x(3,2,1)
         x = (1.0, 3.0)
       end subroutine extsub_arr3
       
   
