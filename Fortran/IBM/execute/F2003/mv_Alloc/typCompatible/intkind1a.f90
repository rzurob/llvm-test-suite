! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of type integer(1)
!*                               TO is of unlimited poly
!*                               move_alloc is called in external procedure
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      module m
          integer(1), allocatable, save   ::  i1(:,:,:)

          interface sub
              subroutine s1(arg)
                   class(*), allocatable, intent(inout) :: arg(:,:,:)
              end subroutine
         end interface

      end module


      use m

      integer(1) i
      type base
          class(*), allocatable   ::  i1(:,:,:)
      end type
      type(base) b1

      allocate(i1(3,2,1), source = reshape((/(i,i=1_1,6_1)/),(/3,2,1/)))

      call sub(b1%i1)
      if ( .not. allocated(b1%i1) ) error stop 21
      if ( allocated(i1) ) error stop 31
      select type ( p => b1%i1)
          type is (integer(1))
             print *, p(:,1,1)
             print *, p(:,2,1)
          class default
              stop 41
      end select
      end

      subroutine s1(arg)
          use m , only : i1
          class(*), allocatable, intent(inout) :: arg(:,:,:)
          call move_alloc(i1, arg)
      end subroutine
