!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DESCRIPTION                : ASSOCIATE with REAL & REAL*16 expressions
!*  KEYWORD(S)                 : ASSOCIATE,real,real*16, RECURSIVE
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
!*  DESCRIPTION                : Test: ASSOCIATE with expressions
!*                                     with derived types with integer
!*                                     logical, complex, real, byte
!*                                     and character data types
!*                                     using function statement and
!*                                     different -qintsize options.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

@PROCESS CTYPLSS


      program fxass89a
      implicit none

      type asso_type
          sequence
          integer i
          real x
          complex c
          logical l
          byte b
          character*10 ch
      end type asso_type

      type (asso_type) fnc,dummy_arg,actual_arg

      fnc(dummy_arg) = dummy_arg

      logical :: precision_r4
      logical :: precision_x8

      integer i,j
      real r
      complex c
      byte b
      character*10 ch

      r = 1.0
      c = (1.0,1.0)
      i = 5
      j = 8
      b = 127_1

      actual_arg = asso_type(10,r,c,i.eq.j,b,'string')

      associate ( arg => fnc(actual_arg))
      if(arg%i .ne. 10)then
         error stop 1
      endif

      if (.not.precision_r4(arg%x,1.0)) then
         error stop 2
      endif

      if (.not.precision_x8(arg%c,(1.0,1.0))) then
         error stop 3
      endif
      if(arg%l .neqv. .false.)then
         error stop 4
      endif

      if(arg%b .ne. 127_1)then
         error stop 5
      endif

      if(arg%ch .ne.'string')then
         error stop 6
      endif

      end associate

      end
