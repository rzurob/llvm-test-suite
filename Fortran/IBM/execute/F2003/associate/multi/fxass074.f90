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
! %GROUP: fxass074.f
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
!*  TEST CASE NAME             : fxass074.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE, complex
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
!*  DESCRIPTION                : Test: MULTIPLE ASSOCIATE with expressions
!*                                     with complex, complex(4),
!*                                     complex(8,16,32) and double
!*                                     complex data types and do loop.
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

      program fxass74
      implicit none

      complex a / (1.0e0, 2.0e0) /
      complex b / (3.0e0, 4.0e0) /
      complex c

      complex*8 a8 / (1.0e0, 2.0e0) /
      complex*8 b8 / (3.0e0, 4.0e0) /
      complex*8 c8
      
      complex*16 a16 / (1.0d0, 2.0d0) /
      complex*16 b16 / (3.0d0, 4.0d0) /
      complex*16 c16

      complex*32 a32 / (1.0q0, 2.0q0) /
      complex*32 b32 / (3.0q0, 4.0q0) /
      complex*32 c32

      double complex aa / (1.0d0, 2.0d0) /
      double complex bb / (3.0d0, 4.0d0) /
      double complex cc

      logical :: precision_x3, precision_x6, precision_x8

      integer count

!-----------   ASSOCIATE with COMPLEX expressions ----------------

      c = (4.0e0, 6.0e0)   
      do count = 1, 10

      associate ( arg => a , argb => b , argc => c)
         arg = arg + argb
         if (.not.precision_x8(arg,argc)) then
           error stop 1
         endif
      end associate
      a = (1.0e0, 2.0e0) 

      end do

!-----------   ASSOCIATE with COMPLEX*16 expressions ----------------

      c16 = (2.0d0, 3.0d0)   
      do count = 1, 10

      associate ( arg16 => a16 , argb16 => (1.0d0, 1.0d0))
         arg16 = arg16 + argb16
         if (.not.precision_x6(arg16,a16)) then
           error stop 6
         endif
      end associate

      end do

!-----------   ASSOCIATE with COMPLEX*32 expressions ----------------

      c32 = (2.0q0, 3.0q0)   
      do count = 1, 10

      associate ( arg32 => a32 , argb32 => (1.0q0, 1.0q0) )
         arg32 = arg32 + argb32
         if (.not.precision_x3(arg32,a32)) then
           error stop 7
         endif
      end associate

      end do
!-----------   ASSOCIATE with DOUBLE COMPLEX expressions ----------------

      do count = 1, 10

      associate ( arg1 => aa , arg2 => (4.0d0, 6.0d0) )
         arg1 = arg1 + bb
         if (.not.precision_x6(arg1,aa)) then
           error stop 8
         endif
      end associate

      end do

      end
