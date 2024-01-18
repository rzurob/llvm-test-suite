!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
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

      program fxass111
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
      if (arg1 .ne. 1) error stop 5
      end associate

      if (i2 .ne. -1) error stop 6
      associate ( arg2 => i2)
      if (arg2 .ne. -1) error stop 7
      end associate

      if (i4 .ne. 10) error stop 8
      associate ( arg3 => i4)
      if (arg3 .ne. 10) error stop 9
      end associate

      if (i8 .ne. -10) error stop 10
      associate ( arg4 => i8)
      if (arg4 .ne. -10) error stop 11
      end associate

   !-------------------------------
   !  Test real
   !-------------------------------

      if (.not. precision_r8((1.1,2.2),c4)) error stop 13
      associate( arg5 => c4 )
      if (.not. precision_r8((1.1,2.2),arg5)) error stop 14
      end associate

   !-------------------------------
   !  Test complex
   !-------------------------------

      if (.not. precision_x8((1.1D-2,-2.2D3),c8)) error stop 15
      associate( arg6 => c8 )
      if (.not. precision_x8((1.1D-2,-2.2D3),arg6)) error stop 16
      end associate

      if (.not. precision_x6((3.3Q5,0.3Q6),c16)) error stop 17
      associate( arg7 => c16 )
      if (.not. precision_x6((3.3Q5,0.3Q6),arg7)) error stop 18
      end associate

      end
