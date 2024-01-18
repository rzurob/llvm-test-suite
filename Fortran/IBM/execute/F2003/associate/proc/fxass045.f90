!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass045.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass045.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DESCRIPTION                : ASSOCIATE with STATEMENT FUNCTION
!*  KEYWORD(S)                 : ASSOCIATE, drived type w/ different data type
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


      program fxass45a
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

      integer i,j
      real r
      complex c
      logical l
      byte b
      character*10 ch

      logical :: precision_r4, precision_r6, precision_r8
      logical :: precision_x3, precision_x6, precision_x8


      r = 1.0
      c = (1.0,1.0)
      i = 5
      j = 8
      b = 127

      actual_arg = asso_type(10,r,c,i.eq.j,b,'string')

      associate ( arg => fnc(actual_arg))
      if(arg%i .ne. 10)then
         error stop 1
      endif

      if (.not.precision_r4(arg%x,r)) then
         error stop 2
      endif

      if (.not.precision_x8(arg%c,c)) then
         error stop 3
      endif

      if(arg%l .neqv. .false.)then
         error stop 4
      endif

      if(arg%b .ne. 127)then
         error stop 5
      endif

      if(arg%ch .ne.'string')then
         error stop 6
      endif

      end associate

      end
