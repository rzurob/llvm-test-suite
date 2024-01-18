!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass103.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass103.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,dimension,logical
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
!*                                     single and double dimention array
!*                                     with using reshape and do loop
!*                                     with logical, logical*1, logical*2
!*                                     logical*4, logical*8 data types.
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


      program fxass103
      implicit none


      logical, dimension(2) ::  log, res_l

      logical*1, dimension(3) :: log1

      logical*2, dimension(4) :: log2

      logical*4, dimension(5) :: log4

      log( 1 ) = .true.
      log( 2 ) = .false.

      log1( 1 ) = .true.
      log1( 2 ) = .false.
      log1( 3 ) = .true.

      log2( 1 ) = .false.
      log2( 2 ) = .false.
      log2( 3 ) = .false.
      log2( 4 ) = .false.

      log4( 1 ) = .true.
      log4( 2 ) = .false.
      log4( 3 ) = .true.
      log4( 4 ) = .false.
      log4( 5 ) = .false.

      associate ( arg => (log(1) .eqv. log(2)) )
           if(arg .neqv. .false.)then
           error stop 1
           endif
      end associate


      associate ( arg1 => (log1(1) .eqv. log1(2) .eqv. log1(3)) )
           if(arg1 .neqv. .false.)then
           error stop 2
           endif
      end associate


      associate ( arg2 => (log2(1) .eqv. log2(2) .eqv. log2(3) .eqv. log2(4)) )
           if(arg2 .neqv. .true.)then
           error stop 3
           endif
      end associate


      associate ( arg4 => (log4(1) .eqv. log4(2) .eqv. log4(3) .eqv. log4(4)) )
           if(arg4 .neqv. (log4(1) .eqv. log4(2) .eqv. log4(3) .eqv. log4(4)))then
           error stop 4
           endif
      end associate

      end
