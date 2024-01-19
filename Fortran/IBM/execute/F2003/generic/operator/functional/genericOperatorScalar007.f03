!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with class hierarchy and unlimited polymorphic
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
      integer :: y = +999
      contains
         generic :: operator(+) => add
         procedure :: add
   end type

   type, extends(base) :: child
      integer :: z = +999
      contains
         procedure :: add => addchild
         generic :: operator(+) => add
   end type

   type, extends(child) :: gen3
      integer :: w
      contains
         procedure :: add => addgen3
         generic :: operator(+) => add
   end type

   contains

   class(base) function add ( a, b )
      class(base), intent(in) :: a, b
      allocatable :: add
      allocate ( base :: add )
      add%x = a%x +b%x
      add%y = a%y +b%y

   end function

    class(base) function addchild ( a, b )
      class(base), intent(in)  ::  b
      class(child), intent(in) ::  a
      allocatable :: addchild

      allocate ( child :: addchild )
      select type ( addchild )
         class is ( child )
            select type ( b )
               class is ( child )
                  addchild%base = a%base + b%base
                  addchild%z = a%z + b%z
            end select
      end select

   end function

    class(base) function addgen3 ( a, b )
      class(base), intent(in) ::  b
      class(gen3), intent(in) ::  a
      allocatable :: addgen3

      select type ( b )
         class is ( gen3 )
            allocate ( addgen3, source = gen3 ( base = a%base + b%base, z = a%z + b%z , w = a%w + b%w ) )
      end select
   end function

end module

program genericOperatorScalar007
  use m

   class(*) :: u1, u2, u3
   allocatable :: u1
   pointer :: u2, u3

   allocate ( u1, source = base(10,10) + base(1,1) )
   select type ( u1 )
      class is ( base )
         print *, u1%x, u1%y
         allocate ( u2, source = base(11,11) + u1 )
         select type ( u2 )
            class is ( base )
               print *, u2%x, u2%y
         end select
   end select

   select type ( u1 )
      class is ( base )
         select type ( u2 )
            class is ( base )
               allocate ( u3, source = u2 + u1 )
               select type ( u3 )
                  class is ( base )
                     print *, u3%x, u3%y
               end select
         end select
   end select

   deallocate ( u1, u2, u3 )

   allocate ( u1, source = child(10,10,10) + child(1,1,1) )

   select type ( u1 )
      type is ( child )
         print *, u1%x, u1%y, u1%z
   end select

   select type ( u1 )
      class is ( base )
         allocate ( u2, source = child(11,11,11) + u1 )
         select type ( u2 )
            class is ( child )
               print *, u2%x, u2%y, u2%z
         end select
   end select

   select type ( u1 )
      class is ( base )
         select type ( u2 )
            class is ( base )
               allocate ( u3, source = u2 + u1 )
               select type ( u3 )
                  class is ( child )
                     print *, u3%x, u3%y, u3%z
               end select
         end select
   end select

   deallocate ( u1, u2, u3 )

   allocate ( u1, source = gen3(10,10,10,10) + gen3(1,1,1,1) )

   select type ( u1 )
      type is ( gen3 )
         print *, u1%x, u1%y, u1%z, u1%w
   end select

   select type ( u1 )
      class is ( base )
         allocate ( u2, source = gen3(11,11,11,11) + u1 )
         select type ( u2 )
            class is ( gen3 )
               print *, u2%x, u2%y, u2%z, u2%w
         end select
   end select

   select type ( u1 )
      class is ( base )
         select type ( u2 )
            class is ( base )
               allocate ( u3, source = u2 + u1 )
               select type ( u3 )
                  class is ( gen3 )
                     print *, u3%x, u3%y, u3%z, u3%w
               end select
         end select
   end select

end program
