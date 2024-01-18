!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: testing the statement: (pg. 62 ln 1-2)
!*                                         If a generic binding specified in a type definition has the same generic-spec as an inherited binding, it
!*                                         extends the generic interface and shall satisfy the requirements specified in 16.2.3.
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
      integer i
      contains
         generic :: operator(+) => add
         procedure :: add
   end type

   type, extends(base) :: child
      integer :: j
      contains
         generic :: operator(+) => addint
         procedure :: addint
   end type

   contains

   class(base) function add (a, b)
      class(base), intent(in) :: a, b
      allocatable add

      allocate ( add, source = a )

      select type ( b )
         type is ( base )
            add%i = add%i + b%i
         type is ( child )
            select type ( add )
               type is ( base )
                  add%i = add%i + b%i
               type is ( child )
                  add%i = add%i + b%i
                  add%j = add%j + b%j
            end select
      end select

   end function

   class(child) function addint (a, b)
      class(child), intent(in) :: a
      integer, intent(in)     :: b
      allocatable addint

      allocate ( addint, source = a )

      addint%i = addint%i + b
      addint%j = addint%j + b

   end function

end module

program genericOperatorInheritance001
   use m

   class(base), allocatable :: b1, b2

   type (child) :: c1
   class(child), allocatable :: c2

   allocate ( b1, source = base(10) )
   allocate ( b2, source = b1 + base(10))

   print *, b1%i, b2%i
   deallocate ( b1, b2 )

   c1 = child ( 10, 11 )
   allocate ( c2, source = child ( 20, 21 ) )

   allocate ( b1, source = child ( 1, 10 ) + child ( 2, 20 ) )  !<- extended type using parent type generic type bound
   allocate ( b2, source = c1 + c2 )

   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   select type ( b2 )
      type is ( child )
         print *, b2%i, b2%j
   end select

   deallocate ( c2 )
   allocate ( c2, source = c1 + 10 )
   print *, c2%i, c2%j

end program
