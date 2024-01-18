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
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: dummy arguments of poly assumed shape array
!*                                           with child type overridding some tb
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
      integer :: i =0
      contains
         procedure, pass :: bassgn
         procedure, pass :: bassgn2d
         generic :: assignment(=) => bassgn, bassgn2d
   end type

   type, extends(base) :: child
      integer :: j = 0
      contains
         procedure, pass :: bassgn2d => cassgn2d
   end type

   contains

      subroutine cassgn2d ( a, b )
         class(child), intent(out) :: a
         class(base), intent(in) :: b(:,:)

         print *, 'cassgn2d'


         do k = 1, size(b,2)
            do j = 1, size(b,1)
               a%i = a%i + b(j,k)%i
            end do
         end do

         if ( same_type_as ( a, b ) ) then
            select type ( b )
               type is ( child )
                  do k = 1, size(b,2)
                     do j = 1, size(b,1)
                        a%j = a%j + b(j,k)%j
                     end do
                  end do
            end select
         end if

      end subroutine

      subroutine bassgn2d ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b(:,:)

         print *, 'bassgn2d'

         do k = 1, size(b,2)
            do j = 1, size(b,1)
               a%i = a%i + b(j,k)%i
            end do
         end do

      end subroutine

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b(:)

         print *, 'bassgn'

         do k = 1, size(b)
            a%i = a%i + b(k)%i
         end do

         select type ( a )
            type is ( child )
               if ( same_type_as ( a, b ) ) then
               	  select type ( b )
               	     type is ( child )
                        do k = 1, size(b)
                           a%j = a%j + b(k)%j
                        end do
                  end select
               end if
         end select

      end subroutine

end module

program genericAssignmentArray008
   use m

   class(base), allocatable :: b1, b2(:), b3(:,:)

   class(child), pointer :: c1, c2(:), c3(:,:)

   allocate ( b1, c1, b2(5), c2(7), b3(2,2), c3(3,3) )

   b1%i = 100
   c1%i = 200
   c1%j = 300

   do k = 1, 5
      b2(k)%i = 10*k
   end do

   do k = 1, 7
      c2(k)%i = 1000*k
      c2(k)%j = -1000*k
   end do

   do k = 1, 2
      do j = 1, 2
         b3(j,k)%i = j*k
      end do
   end do

   do k = 1, 3
      do j = 1, 3
         c3(j,k)%i = j*k
         c3(j,k)%j = -1*j*k
      end do
   end do

   b1 = b2
   print *, b1%i

   c1 = c2
   print *, c1%i, c1%j

   b1 = c2
   print *, b1%i

   c1 = b2
   print *, c1%i, c1%j

   b1 = b3
   print *, b1%i

   c1 = c3
   print *, c1%i, c1%j

   deallocate ( b1 )
   allocate ( child :: b1 )

   b1 = c2
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   b1 = b2
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   b1 = c3
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   b1 = b3
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

end program
