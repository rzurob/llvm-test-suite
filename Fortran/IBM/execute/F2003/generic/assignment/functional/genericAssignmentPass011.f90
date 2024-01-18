!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified with array dummy arg and elemental
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
      integer(4) :: i = -999
      contains
         procedure, pass(b) :: array_scalar
         procedure, pass    :: scalar_array
         procedure, pass    :: elemental
         generic :: assignment(=) => scalar_array,array_scalar,elemental
   end type

   type, extends(base) :: child
      integer(4) :: j = -999
      contains
         procedure, pass(b) :: array_scalar => array_Cscalar
         procedure, pass    :: scalar_array => Cscalar_array
         procedure, pass    :: elemental => Celemental
   end type

   contains

   subroutine array_scalar ( a, b )
      class(base), intent(out) :: a(:)
      class(base), intent(in)  :: b

      do i =0, size(a)-1
         a(i+1)%i = b%i + i
      end do

      print *,'array_scalar'

   end subroutine

   subroutine scalar_array ( a, b )
      class(base), intent(out) :: a
      class(base), intent(in)  :: b(:)

      a%i = b(1)%i

      do i =2, size(b)
         a%i = b(i)%i + a%i
      end do

      print *,'scalar_array'

   end subroutine

   elemental subroutine elemental ( a, b )
      class(base), intent(out) :: a
      class(base), intent(in)  :: b

      a%i = b%i + 1

   end subroutine

   subroutine array_Cscalar ( a, b )
      class(base), intent(out) :: a(:)
      class(child), intent(in)  :: b

      do i =0, size(a)-1
         a(i+1)%i = b%i + i
      end do

      select type ( a )
         type is ( child )
            do i =0, size(a)-1
               a(i+1)%j = b%j + i
            end do
      end select

      print *,'array_Cscalar'

   end subroutine

   subroutine Cscalar_array ( a, b )
      class(child), intent(out) :: a
      class(base), intent(in)  :: b(:)

      a%i = b(1)%i

      do i=2, size(b)
         a%i = b(i)%i + a%i
      end do

      select type ( b )
         type is ( child )
            a%j = b(1)%j

            do i=2, size(b)
               a%j = b(i)%j + a%j
            end do
      end select

      print *,'Cscalar_array'

   end subroutine

   elemental subroutine Celemental ( a, b )
      class(child), intent(out) :: a
      class(base), intent(in)  :: b

      a%i = b%i + 1

      select type ( b )
         type is ( child )
            a%j = b%j + 1
      end select

   end subroutine

end module

program genericAssignmentPass011
   use m

   class(base), allocatable :: b1, b2(:)
   class(child), allocatable :: c1, c2(:)

   allocate ( b1, b2(3), c1, c2(3) )

   b1 = base(100)
   print *, b1%i

   b2 = b1
   print *, b2%i

   c2 = (/ child(100,1000), child(200,2000), child(300,3000) /)
   print *, c2%i, c2%j

   c1 = c2
   print *, c1%i, c1%j

   b1 = b2
   print *, b1%i

   c2 = c1
   print *, c2%i, c2%j

   b2 = c2
   print *, b2%i

   c2 = b2
   print *, c2%i, c2%j

   deallocate ( b1, b2 )
   allocate ( child :: b2(4), b1 )

   b2 = (/ (child(i*100,i*1000),i=1,4) /)

   select type ( b2 )
      type is ( child )
         print *, b2%i, b2%j
   end select

   b1 = b2
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   c2 = b1
   print *, c2%i, c2%j

   b2 = b1
   select type ( b2 )
      type is ( child )
         print *, b2%i, b2%j
   end select

   c1 = b2
   print *, c1%i, c1%j

end program
