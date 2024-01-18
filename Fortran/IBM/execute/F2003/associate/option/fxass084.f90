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
! %COMPOPTS: -qfree=f90 -qintsize=4
! %GROUP: fxass084.f
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
!*  TEST CASE NAME             : fxass084.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE ,allocate
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE, allocate
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
!*  DESCRIPTION                : Test: ASSOCIATE with allocatable
!*                                     array section with real,integer
!*                                     data types. using intrinsic function
!*                                     ubound.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass084
      implicit none

      integer    i, j
      logical    precision_r4

      integer,   allocatable :: int1(:, :)
      real, allocatable :: rel1(:, :)

      allocate   (int1(2:11, -4:5))
      allocate   (rel1(10, 10))

      int1 = 0
      rel1 = 1.0

      forall(i=2:11, j=-4:5, i==1) int1(i,j)= 9
      forall(i=1:10, j=1:10, j>=2 .and. j<=6) rel1(i,j)= 9.0

      associate( alloc_i => ubound(int1,1) )
        if (alloc_i .ne. 11) error stop 1        
      end associate

      associate( alloc_r => ubound(rel1,1) )
        if (alloc_r .ne. 10) error stop 2        
      end associate

      associate( alloc => int1(2,3)*4 )
        if (alloc .ne. (int1(2,3)*4)) error stop 3        
      end associate

      associate( alloc => rel1(2,3)*4 )
        if (.not. precision_r4(alloc,(rel1(2,3)*4))) error stop 4
      end associate

      associate( alloc => int1(2,3) )
        alloc = alloc + 5  
        if (alloc .ne. int1(2,3)) error stop 5        
      end associate

      associate( alloc => rel1(2,3) )
        alloc = alloc / 2
        if (.not. precision_r4(alloc,rel1(2,3))) error stop 6
      end associate
      end

