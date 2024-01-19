! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of type integer(1)
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      module m
          integer(1), allocatable   ::  i1(:,:,:)
          type base
              integer(1), allocatable   ::  i1(:,:,:)
          end type
      end module


      use m

      type(base) b1
      integer(1) i

      allocate(b1%i1(3,2,1), source = reshape((/(i,i=1_1,6_1)/),(/3,2,1/)))

      call move_alloc(b1%i1, i1)

      if ( allocated(b1%i1) ) error stop 21
      if ( .not. allocated(i1) ) error stop 31
      print *, i1(:,1,1)
      print *, i1(:,2,1)
      end
