!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on allocate
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,integer,logical
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
!*                                     integer*(1,2,4,8), logical(1,2,4,8)
!*                                     and ALLOCATE with
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass112
      implicit none

      integer*1, allocatable :: i1
      integer*2, allocatable :: i2
      integer*4, allocatable :: i4
      integer*8, allocatable :: i8

      logical(1),allocatable :: log1
      logical(2),allocatable :: log2
      logical(4),allocatable :: log4
      logical(8),allocatable :: log8

      allocate (i1, i2, i4, i8)
      allocate (log1,log2,log4,log8)

      i1 = 100
      i2 = i1
      i4 = i2
      i8 = i4

      log1 = .true.
      log2 = .false.
      log4 = .true.
      log8 = .false.

      if (i1 .ne. 100) error stop 4
      associate ( arg1 => i1)
      if (arg1 .ne. 100) error stop 5
      end associate

      if (i2 .ne. 100) error stop 6
      associate ( arg2 => i2)
      if (arg2 .ne. 100) error stop 7
      end associate

      if (i4 .ne. 100) error stop 8
      associate ( arg3 => i4)
      if (arg3 .ne. 100) error stop 9
      end associate

      if (i8 .ne. 100) error stop 10
      associate ( arg4 => i8)
      if (arg4 .ne. 100) error stop 11
      end associate

      if (.not. log1) error stop 13
      associate ( arg5 => log1 )
      if (.not. arg5) error stop 13
      end associate

      if (log2) error stop 14
      associate ( arg6 => log2 )
      if (arg6) error stop 14
      end associate

      if (.not.log4) error stop 15
      associate ( arg7 => log4 )
      if (.not.arg7) error stop 15
      end associate

      if (log8) error stop 16
      associate ( arg8 => log8 )
      if (arg8) error stop 16
      end associate

      deallocate (i1)
      deallocate (i2)
      deallocate (i4)
      deallocate (i8)

      deallocate (log1)
      deallocate (log2)
      deallocate (log4)
      deallocate (log8)

      if(allocated(i1) .or. allocated(i2) .or. &
         allocated(i4) .or. allocated(i8)) then
         error stop 17
      endif

      if(allocated(log1) .or. allocated(log2) .or. &
       allocated(log4) .or. allocated(log8)) then
       error stop 18
      endif

      end
