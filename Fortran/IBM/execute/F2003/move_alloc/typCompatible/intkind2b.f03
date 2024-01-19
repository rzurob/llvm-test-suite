! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of type integer(2)
!*                               default kind becomes 2 by -qintsize = 2
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      integer(2), allocatable   ::  j1(:)
      type base
          integer, allocatable   ::  i1(:)
      end type

      type(base) b1
      allocate(j1(3), source = (/ 30_2, 20_2, 10_2 /) )

      call move_alloc(j1, b1%i1)
      if ( .not. allocated( b1%i1) ) error stop 21
      if ( allocated( j1 ) ) error stop 23

      print *, b1%i1

      end
