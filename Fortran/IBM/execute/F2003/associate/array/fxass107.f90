!**********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!**********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90
! %GROUP: fxass107.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass107.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,dimension,real
!*  TARGET(S)                  :
!*  NUMBER OF TESTS            : 1
!*  STATUS                     : done
!*
!*  STRUCTURE                  : Main program
!*  EXECUTABLE                 : Yes
!*
!*  INPUTS                     : None
!*  OUTPUTS                    : None
!*
!*  SETUP REQUIREMENTS         : N/A
!*  DEPENDENCIES               : External routine ZZRC
!*  REQUIRED COMPILER OPTIONS  : None
!*
!*  NORMAL COMPLETION          : Return code = 0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  CONDITIONS TESTED          : Listed below.
!*
!*  DESCRIPTION                : Test: ASSOCIATE with function and with
!*                                     single dimention array
!*                                     with real, real*4, real*8, real*16
!*                                     data types.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass107
      implicit none

      real, dimension(2) :: arr_r, ar_r
      real*4, dimension(3) :: arr_r1, ar_r1
      real*8, dimension(4) :: arr_r2, ar_r2
      real*16, dimension(5) :: arr_r4, ar_r4

      logical :: precision_r4, precision_r8, precision_r6

      arr_r = (/ 1.0,2.0 /)

      arr_r1 = (/ 1.0,2.0,3.0 /)

      arr_r2 = (/ 1.0,2.0,3.0,4.0 /)

      arr_r4 = (/ 1.0,2.0,3.0,4.0,5.0 /)

      associate ( arr1 => rel_fun(arr_r) )
           if (.not.precision_r4(arr1,arr_r(1))) error stop 1
      end associate

      associate ( arr3 => rel1_fun(arr_r1) )
           if (.not.precision_r4(arr3,arr_r1(1))) then
           error stop 4
           endif
      end associate

      associate ( arr6 => rel2_fun(arr_r2) )
           if (.not.precision_r8(arr6,arr_r2(3))) then
           error stop 8
           endif
      end associate

      associate ( arr10 => rel4_fun(arr_r4) )
           if (.not.precision_r6(arr10,arr_r4(4))) then
           error stop 13
           endif
      end associate



      contains

      real function rel_fun(arr_r)
      real, dimension(2) :: arr_r
      rel_fun = arr_r(1)
      end function

      real*4 function rel1_fun(arr_r1)
      real*4, dimension(3) :: arr_r1
      rel1_fun = 1.0
      end function

      real*8 function rel2_fun(arr_r2)
      real*8, dimension(4) :: arr_r2
      rel2_fun = arr_r2(3)
      end function

      real*16 function rel4_fun(arr_r4)
      real*16, dimension(5) :: arr_r4
      rel4_fun = arr_r4(4)
      end function

      end
