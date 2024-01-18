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
         procedure, pass, private :: returnBase
         procedure, pass, private :: returnBaseArray
         generic :: return => returnBase, returnBaseArray
   end type

   type, extends(base) :: child
      integer :: k
   end type

   contains

   class(base) function returnBase(a)
      class(base), intent(in) :: a
      allocatable :: returnBase

      allocate ( returnBase, source = a )
      print *,'returnbase'

   end function

   class(base) function returnBaseArray(a,j)
      class(base), intent(in) :: a
      integer, intent(in) :: j
      allocatable :: returnBaseArray(:)

      allocate ( returnBaseArray(j), source = (/ (a, i = 1, j ) /) )
      print *,'returnbasearray'
   end function

end module

program genericGenericNameScalar017
   use m

   class(base), pointer :: b1, b3(:)
   class(base), allocatable :: b2

   class(child), pointer :: c1

   allocate ( b1, source = base(100))
   allocate ( b2 , source = b1%return() )

   print *, b1%j
   print *, b2%j

   allocate ( b3(5), source = b1%return(5) )

   print *, b3%j

   deallocate ( b2, b3 )

   allocate ( c1, source = child(50, 100) )
   print *, c1%j, c1%k

   allocate ( b2, source = c1%return() )
   select type ( b2 )
      type is ( child )
         print *, b2%j, b2%k
   end select

   allocate ( b3(10), source = c1%return(10) )
   select type ( b3 )
      type is ( child )
         print *, b3%j
         print *, b3%k
   end select

end program
