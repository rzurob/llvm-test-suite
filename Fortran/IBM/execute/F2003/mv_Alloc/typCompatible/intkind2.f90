! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of type integer(2)
!*                               TO is of unlimit poly
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      class(*), allocatable   ::  j2(:,:,:,:)
      integer(2), allocatable   ::  i2(:,:,:,:)
      integer(2), parameter :: con = 0
      integer(2) i

      allocate(integer*2 :: j2(0,0,-0,con))
      allocate(i2(1,2,2,1), source=reshape((/ (con-i,i=1,4)/), (/1,2,2,1/)))
      call move_alloc(i2, j2)

      if ( allocated(i2) ) stop 21
      if ( .not. allocated(j2) ) stop 31

      select type (j2)
          type is (integer*2)
              print *, "j2(",shape(j2), ") = ", j2
          class default
              stop 21
      end select

      end
