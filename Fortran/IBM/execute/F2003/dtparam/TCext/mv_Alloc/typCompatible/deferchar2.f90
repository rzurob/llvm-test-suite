! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mv_Alloc/typCompatible/deferchar2.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : deferchar2.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : FROM/TO are of deferred character type 
!*                               len = 0 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      character(len=:), allocatable :: ch1(:)
      character(-2), parameter :: char = 'IBM' 

      type A(k1,n1)    ! (4,20) 
          integer, kind :: k1
          integer, len  :: n1
          character(len=len(char)), allocatable :: ch4(:)
      end type

      type(A(4,20)) :: aT 

      allocate(aT%ch4(3))

      call move_alloc(aT%ch4 , ch1 )

      if ( .not. allocated(ch1) ) stop 21 
      if ( allocated(aT%ch4) ) stop 23
      if ( len(ch1(1)) /= 0 ) stop 25

      end
