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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: with elemental function with class hierarchy
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
      integer :: x = -999
      contains
         generic :: operator(*) => mul
         procedure :: mul
   end type

   type, extends(base) :: child
      integer :: y = -999
      contains
         procedure :: mulchild
         generic :: operator(*) => mulchild
   end type

   contains

      elemental type(base) function mul ( a, b )
         class(base), intent(in) :: a
         type(base), intent(in) :: b

         mul%x = a%x * b%x

      end function

      elemental type(child) function mulchild ( a, b )
         class(child), intent(in) :: a
         type(child), intent(in) :: b

         mulchild%base = a%base * b%base
         mulchild%y = a%y * b%y

      end function

end module

program genericOperatorElemental002
   use m

   type(base) :: b1, b2(3)
   class(base), allocatable :: b3(:)

   type(child) :: c1(3)
   class(child), pointer :: c2(:)

   b1 = base(2)
   b2 = (/ base(3),  base(4),  base(5) /)

   allocate ( b3(3), source = ( b1 * b2 ) )
   print *, b3%x

   c1 = (/ child(1,2), child(3,4), child(5,6) /)
   deallocate ( b3 )
   allocate ( b3(3), source =  b1 * c1%base )

   print *, b3%x

   deallocate ( b3 )
   allocate ( c2(3),source = (/ (child (i, i+1) , i = 2,6,2 ) /) )
   allocate ( b3(3), source = (/ c1 * c2 /) )

   select type ( b3 )
      type is ( child )
         print *, b3%x
         print *, b3%y
   end select

   deallocate ( b3, c2 )
   allocate ( c2(3), source = (/ ( child(j,j*2), j = 3,1,-1 ) /) * (/ ( child(j,j*2), j = 1,3 ) /) )

   print *, c2%x
   print *, c2%y

   allocate ( b3(2), source = child(1,2) * (/ child(2,3), child(3,4) /) )

   select type ( b3 )
      type is ( child )
         print *, b3%x
         print *, b3%y
   end select

end program
