! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorScalar007.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: x = -999
      integer(k1)   :: y = +999
      contains
         generic :: operator(+) => add
         procedure :: add
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: z = +999
      contains
         procedure :: add => addchild
         generic :: operator(+) => add
   end type

   type, extends(child) :: gen3    ! (20,4)
      integer(k1) :: w
      contains
         procedure :: add => addgen3
         generic :: operator(+) => add
   end type

   contains

   class(base(:,4)) function add ( a, b )
      class(base(*,4)), intent(in) :: a, b
      allocatable :: add
      allocate ( base(20,4) :: add )
      add%x = a%x +b%x
      add%y = a%y +b%y

   end function

    class(base(:,4)) function addchild ( a, b )
      class(base(*,4)), intent(in)  ::  b
      class(child(*,4)), intent(in) ::  a
      allocatable :: addchild

      allocate ( child(20,4) :: addchild )
      select type ( addchild )
         class is ( child(*,4) )
            select type ( b )
               class is ( child(*,4) )
                  addchild%base = a%base + b%base
                  addchild%z = a%z + b%z
            end select
      end select

   end function

    class(base(:,4)) function addgen3 ( a, b )
      class(base(*,4)), intent(in) ::  b
      class(gen3(*,4)), intent(in) ::  a
      allocatable :: addgen3

      select type ( b )
         class is ( gen3(*,4) )
            allocate ( addgen3, source = gen3(20,4) ( base = a%base + b%base, z = a%z + b%z , w = a%w + b%w ) )
      end select
   end function

end module

program genericOperatorScalar007
  use m

   class(*) :: u1, u2, u3
   allocatable :: u1
   pointer :: u2, u3

   allocate ( u1, source = base(20,4)(10,10) + base(20,4)(1,1) )
   select type ( u1 )
      class is ( base(*,4) )
         print *, u1%x, u1%y
         allocate ( u2, source = base(20,4)(11,11) + u1 )
         select type ( u2 )
            class is ( base(*,4) )
               print *, u2%x, u2%y
         end select
   end select

   select type ( u1 )
      class is ( base(*,4) )
         select type ( u2 )
            class is ( base(*,4) )
               allocate ( u3, source = u2 + u1 )
               select type ( u3 )
                  class is ( base(*,4) )
                     print *, u3%x, u3%y
               end select
         end select
   end select

   deallocate ( u1, u2, u3 )

   allocate ( u1, source = child(20,4)(10,10,10) + child(20,4)(1,1,1) )

   select type ( u1 )
      type is ( child(*,4) )
         print *, u1%x, u1%y, u1%z
   end select

   select type ( u1 )
      class is ( base(*,4) )
         allocate ( u2, source = child(20,4)(11,11,11) + u1 )
         select type ( u2 )
            class is ( child(*,4) )
               print *, u2%x, u2%y, u2%z
         end select
   end select

   select type ( u1 )
      class is ( base(*,4) )
         select type ( u2 )
            class is ( base(*,4) )
               allocate ( u3, source = u2 + u1 )
               select type ( u3 )
                  class is ( child(*,4) )
                     print *, u3%x, u3%y, u3%z
               end select
         end select
   end select

   deallocate ( u1, u2, u3 )

   allocate ( u1, source = gen3(20,4)(10,10,10,10) + gen3(20,4)(1,1,1,1) )

   select type ( u1 )
      type is ( gen3(*,4) )
         print *, u1%x, u1%y, u1%z, u1%w
   end select

   select type ( u1 )
      class is ( base(*,4) )
         allocate ( u2, source = gen3(20,4)(11,11,11,11) + u1 )
         select type ( u2 )
            class is ( gen3(*,4) )
               print *, u2%x, u2%y, u2%z, u2%w
         end select
   end select

   select type ( u1 )
      class is ( base(*,4) )
         select type ( u2 )
            class is ( base(*,4) )
               allocate ( u3, source = u2 + u1 )
               select type ( u3 )
                  class is ( gen3(*,4) )
                     print *, u3%x, u3%y, u3%z, u3%w
               end select
         end select
   end select

end program
