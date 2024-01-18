! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameDeferred003.f
! opt variations: -qnok -ql -qreuse=none

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
!*  DESCRIPTION                : generic-name: generic tb with some specific and deferred binding with different rank
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

   type, abstract :: base(k1)    ! (4)
       integer, kind :: k1
      contains
         procedure, pass :: scalar
         procedure(face1), deferred, pass :: onedarray
         procedure(face2), deferred :: twodarray

         generic :: copy => scalar, onedarray, twodarray

   end type
   
   type, extends(base) :: child    ! (4)
      integer(k1) :: i
      contains
         procedure :: scalar => scalarchild
         procedure :: onedarray
         procedure :: twodarray
   end type

   abstract interface
      subroutine face0 (a, b)
         import base
         class(base(4)), intent(inout) :: a
         class(base(4)), intent(in) :: b
      end subroutine
   end interface

   abstract interface
      subroutine face1 (a, b)
         import base
         class(base(4)), intent(inout) :: a
         class(base(4)), intent(in) :: b(:)
      end subroutine

      subroutine face2 (a, b)
         import base
         class(base(4)), intent(inout) :: a
         class(base(4)), intent(in) :: b(:,:)
      end subroutine
   end interface

   contains

      subroutine scalar( a, b )
         class(base(4)), intent(inout) :: a
         class(base(4)), intent(in) :: b

         print *, 'scalar'
      end subroutine

       subroutine scalarchild( a, b )
         class(child(4)), intent(inout) :: a
         class(base(4)), intent(in) :: b

         select type ( b )
            type is ( child(4) )
               a%i = b%i
         end select

         print *, 'scalarchild'
      end subroutine

      subroutine onedarray( a, b )
         class(child(4)), intent(inout) :: a
         class(base(4)), intent(in) :: b(:)

         select type ( b )
            type is ( child(4) )
               a%i = b(1)%i
               do i = 2, size(b)
                  a%i = a%i + b(i)%i
               end do
         end select

         print *, 'onedarray'
      end subroutine

      subroutine twodarray( a, b )
         class(child(4)), intent(inout) :: a
         class(base(4)), intent(in) :: b(:,:)

         select type ( b )
            type is ( child(4) )
               a%i = b(1,1)%i
               do i = 2, size(b,1)
                  do j = 2, size(b,2)
                     a%i = a%i + b(i,j)%i
                  end do
               end do
         end select

         print *, 'twodarray'

      end subroutine

end module

program genericGenericNameDeferred003
   use m

   class(base(4)), allocatable :: b1, b2(:), b3(:,:)
   type(child(4)), pointer :: c1, c2(:), c3(:,:)

   allocate ( b1, source = child(4)(100) )
   allocate ( b2(3), source = (/ child(4)(200), child(4)(300), child(4)(400) /) )
   allocate ( b3(2,2), source = reshape ( source = (/ child(4)(500), child(4)(600), child(4)(700), child(4)(800) /), shape = (/2,2/) ) )

   allocate ( c1, source = child(4)(1000) )
   allocate ( c2(3), source = (/ child(4)(2000), child(4)(3000), child(4)(4000) /) )
   allocate ( c3(2,2), source = reshape ( source = (/ child(4)(5000), child(4)(6000), child(4)(7000), child(4)(8000) /), shape = (/2,2/) ) )

   call b1%copy(c1)
   select type ( b1 )
      type is  ( child(4) )
         print *, b1%i
   end select

   call b1%copy(c2)
   select type ( b1 )
      type is  ( child(4) )
         print *, b1%i
   end select

   call b1%copy(c3)
   select type ( b1 )
      type is  ( child(4) )
         print *, b1%i
   end select

   deallocate ( b1 )
   allocate ( b1, source = child(4)(100) )

   call c1%copy(b1)
   print *, c1%i
   call c1%copy(b2)
   print *, c1%i
   call c1%copy(b3)
   print *, c1%i

   call c1%copy(c1)
   print *, c1%i
   call c1%copy(c2(1:2))
   print *, c1%i
   call c1%copy(c3(2:1:-1,2:1:-1))
   print *, c1%i

   call b1%copy(b1)
   select type ( b1 )
      type is  ( child(4) )
         print *, b1%i
   end select

   call b1%copy(b2((/3,2,2,2,1/)))
   select type ( b1 )
      type is  ( child(4) )
         print *, b1%i
   end select

   call b1%copy(b3)
   select type ( b1 )
      type is  ( child(4) )
         print *, b1%i
   end select

end program
