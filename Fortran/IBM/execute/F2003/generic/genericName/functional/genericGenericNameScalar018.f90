!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic binding containing dummy args of different ranks
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m

   type base
      integer :: j
      contains
         procedure, nopass :: noarg
         procedure, nopass :: scalarbase
         procedure, nopass :: oneDbase
         procedure, nopass :: twoDbase
         procedure, nopass :: threeDbase
         procedure, nopass :: fourDbase

         generic :: anyD => noarg, scalarbase, oneDbase, twoDbase, threeDbase, fourDbase

   end type

   type, extends(base) :: child
      integer :: k
   end type

   contains

      subroutine noarg()

         print *, 'noarg'

      end subroutine

      subroutine scalarbase(a)
         class(base), intent(in) :: a

         print *, 'scalarbase'

      end subroutine

      subroutine oneDbase(a)
         class(base), intent(in) :: a(:)

         print *, 'oneDbase'

      end subroutine

      subroutine twoDbase(a)
         class(base), intent(in) :: a(:,:)

         print *, 'twoDbase'

      end subroutine

      subroutine threeDbase(a)
         class(base), intent(in) :: a(:,:,:)

         print *, 'threeDbase'

      end subroutine

      subroutine fourDbase(a)
         class(base), intent(in) :: a(:,:,:,:)

         print *, 'fourDbase'

      end subroutine

end module

program genericGenericNameScalar018
   use m

   class(base), allocatable :: b0, b1(:), b2(:,:), b3(:,:,:), b4(:,:,:,:)

   ! when unallocated

   call b0%anyD()
   call b0%anyD(b0)
   call b0%anyD(b1)
   call b1%anyD(b1)
   call b1%anyD(b2)
   call b2%anyD(b2)
   call b2%anyD(b3)
   call b3%anyD(b3)
   call b3%anyD(b4)
   call b4%anyD(b4)
   call b4%anyD(b0)

   ! when allocated

   allocate ( b0, b1(1), b2(1,1), b3(1,1,1), b4(1,1,1,1) )

   call b0%anyD()
   call b0%anyD(b0)
   call b0%anyD(b1)
   call b1%anyD(b1)
   call b1%anyD(b2)
   call b2%anyD(b2)
   call b2%anyD(b3)
   call b3%anyD(b3)
   call b3%anyD(b4)
   call b4%anyD(b4)
   call b4%anyD(b0)

   deallocate ( b0, b1, b2, b3, b4)

   allocate ( child :: b0, b1(1), b2(1,1), b3(1,1,1), b4(1,1,1,1) )

   call b0%anyD()
   call b0%anyD(b0)
   call b0%anyD(b1)
   call b1%anyD(b1)
   call b1%anyD(b2)
   call b2%anyD(b2)
   call b2%anyD(b3)
   call b3%anyD(b3)
   call b3%anyD(b4)
   call b4%anyD(b4)
   call b4%anyD(b0)

end program
