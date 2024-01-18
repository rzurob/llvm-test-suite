!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on derived types and allocate
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,integer, derived types
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
!*  DESCRIPTION                : Test: ASSOCIATE with expression and with
!*                                     integer*(1,2,4,8), real*(4,8,16)
!*                                     and ALLOCATE with derived types.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass110
      implicit none

      type dtest

      integer(1),allocatable ::i1
      integer(2),allocatable ::i2
      integer(4),allocatable ::i4
      integer(8),allocatable ::i8

      end type dtest

      type d_test

      real(4),allocatable ::r4
      real(8),allocatable ::r8
      real(16),allocatable ::r16

      end type d_test

      type(dtest) dt
      type(d_test) d_t

      logical precision_r4, precision_r8, precision_r6

      allocate (dt%i1)
      allocate (dt%i2)
      allocate (dt%i4)
      allocate (dt%i8)

      allocate (d_t%r4)
      allocate (d_t%r8)
      allocate (d_t%r16)

      dt%i1 = 1
      dt%i2 = -1
      dt%i4 = 10
      dt%i8 = -10

      d_t%r4 = 10.12
      d_t%r8 = -18.04
      d_t%r16 = -1.0

      associate ( arg1 => dt%i1)
      if (arg1 .ne. dt%i1) error stop 5
      end associate

      associate ( arg2 => dt%i2)
      if (arg2 .ne. dt%i2) error stop 7
      end associate

      associate ( arg3 => dt%i4)
      if (arg3 .ne. dt%i4) error stop 9
      end associate

      associate ( arg4 => dt%i8)
      if (arg4 .ne. dt%i8) error stop 11
      end associate

      associate ( arg5 => d_t%r4 )
      if (.not. precision_r4(d_t%r4,arg5)) error stop 14
      end associate

      associate ( arg6 => d_t%r8 )
      if (.not. precision_r8(d_t%r8,arg6)) error stop 16
      end associate

      associate ( arg7 => d_t%r16 )
      if (.not. precision_r6(d_t%r16,arg7)) error stop 18
      end associate

      deallocate (dt%i1)
      deallocate (dt%i2)
      deallocate (dt%i4)
      deallocate (dt%i8)

      deallocate (d_t%r4)
      deallocate (d_t%r8)
      deallocate (d_t%r16)

      if(allocated(dt%i1) .or. allocated(dt%i2) .or. &
       allocated(dt%i4) .or. allocated(dt%i8)) then
       error stop 19
      endif

      if(allocated(d_t%r4) .or. allocated(d_t%r8) .or. allocated(d_t%r16)) then
      error stop 20
      endif

      end
