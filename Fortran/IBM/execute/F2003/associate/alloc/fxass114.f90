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
! %GROUP: fxass114.f
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
!*  TEST CASE NAME             : fxass114.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on allocate
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,integer, constant array,complex 
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
!*  DESCRIPTION                : Test: ASSOCIATE with expression and with
!*                                     integer*(1,2,4,8) and complex(4,8,16) 
!*                                     and ALLOCATE with constant array.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass114
      implicit none

      integer,parameter, dimension(4) :: arr = (/1,-1,10,-10/)
      integer(1),allocatable ::i1 
      integer(2),allocatable ::i2 
      integer(4),allocatable ::i4 
      integer(8),allocatable ::i8

      complex(4),allocatable :: c4
      complex(8),allocatable :: c8
      complex(16),allocatable :: c16

      logical precision_r8, precision_x8, precision_x6

      allocate (i1,i2,i4,i8)
      allocate (c4,c8,c16)

      i1= arr(1)
      i2= arr(2)
      i4= arr(3)
      i8= arr(4)

      c4 = (1.1,2.2)
      c8 = (1.1D-2,-2.2D3)
      c16 = (3.3Q5,0.3Q6)

   !-------------------------------
   !  Test integer
   !-------------------------------

      if (i1 .ne. 1) error stop 4
      associate ( arg1 => i1)
      arg1 = arg1 + 1
      if (arg1 .ne. 2) error stop 5
      end associate

      if (i2 .ne. -1) error stop 6
      associate ( arg2 => i2)
      arg2 = arg2 * (-1)
      if (arg2 .ne. 1) error stop 7
      end associate

      if (i4 .ne. 10) error stop 8
      associate ( arg3 => i4)
      arg3 = arg3 - 5
      if (arg3 .ne. 5) error stop 9
      end associate

      if (i8 .ne. -10) error stop 10
      associate ( arg4 => i8)
      arg4 = arg4 / 2
      if (arg4 .ne. -5) error stop 11
      end associate

   !-------------------------------
   !  Test real
   !-------------------------------

      if (.not. precision_r8((1.1,2.2),c4)) error stop 13
      associate( arg5 => c4 )
      arg5 = arg5 + (1.1,2.2)
      if (.not. precision_r8((2.2,4.4),arg5)) error stop 14
      end associate

   !-------------------------------
   !  Test complex
   !-------------------------------

      if (.not. precision_x8((1.1D-2,-2.2D3),c8)) error stop 15
      associate( arg6 => c8 )
      arg6 = arg6 * 1
      if (.not. precision_x8((1.1D-2,-2.2D3),arg6)) error stop 16
      end associate

      if (.not. precision_x6((3.3Q5,0.3Q6),c16)) error stop 17
      associate( arg7 => c16 )
      arg7 = arg7 - (1.0,0.0)
      if (.not. precision_x6(c16,arg7)) error stop 18
      end associate


      deallocate (i1,i2,i4,i8)
      deallocate (c4, c8, c16)

      if(allocated(i1) .or. allocated(i2) .or. &
       allocated(i4) .or. allocated(i8)) error stop 19 

      if(allocated(c4) .or. allocated(c8) .or. allocated(c16)) error stop 20

      end 
