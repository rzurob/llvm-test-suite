! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c01i bind_c01i
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
!*
!* DATE                         : Sep. 1, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute
!*                                array of different data types
!*                                Using external subroutine,interface.
!*                                C calls fortran.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890



       subroutine extsub_arr1(a, b) bind(c)
         integer a(3,2,1)
         real    b(3,2,1)
         a = 3
         b = 3.4
       end subroutine extsub_arr1

       subroutine extsub_arr2(c, l) bind(c)
         character*1 c(3,2,1)
         logical*1 l(3,2,1)
         c = 'd'
         l = .true.
       end subroutine extsub_arr2

       subroutine extsub_arr3(x) bind(c)
         complex x(3,2,1)
         x = (1.0, 3.0)
       end subroutine extsub_arr3


