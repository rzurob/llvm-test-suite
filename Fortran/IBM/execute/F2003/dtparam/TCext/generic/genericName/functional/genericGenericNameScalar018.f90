! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar018.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: j
      contains
         procedure, nopass :: noarg
         procedure, nopass :: scalarbase
         procedure, nopass :: oneDbase
         procedure, nopass :: twoDbase
         procedure, nopass :: threeDbase
         procedure, nopass :: fourDbase

         generic :: anyD => noarg, scalarbase, oneDbase, twoDbase, threeDbase, fourDbase

   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: k
   end type

   contains

      subroutine noarg()

         print *, 'noarg'

      end subroutine

      subroutine scalarbase(a)
         class(base(*,4)), intent(in) :: a

         print *, 'scalarbase'

      end subroutine

      subroutine oneDbase(a)
         class(base(*,4)), intent(in) :: a(:)

         print *, 'oneDbase'

      end subroutine

      subroutine twoDbase(a)
         class(base(*,4)), intent(in) :: a(:,:)

         print *, 'twoDbase'

      end subroutine

      subroutine threeDbase(a)
         class(base(*,4)), intent(in) :: a(:,:,:)

         print *, 'threeDbase'

      end subroutine

      subroutine fourDbase(a)
         class(base(*,4)), intent(in) :: a(:,:,:,:)

         print *, 'fourDbase'

      end subroutine

end module

program genericGenericNameScalar018
   use m

   class(base(:,4)), allocatable :: b0, b1(:), b2(:,:), b3(:,:,:), b4(:,:,:,:)

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
   
   allocate ( base(20,4):: b0, b1(1), b2(1,1), b3(1,1,1), b4(1,1,1,1) )
   
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
   
   allocate ( child(20,4) :: b0, b1(1), b2(1,1), b3(1,1,1), b4(1,1,1,1) )

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
