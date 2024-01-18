! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorScalar006.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with class hierarchy
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: x = -999
      integer(k1)   :: y = -999
      contains
         generic :: operator(-) => sub
         procedure :: sub
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: z = -999
      contains
         procedure :: sub => subchild
         generic :: operator(-) => sub
   end type

   type, extends(child) :: gen3    ! (4)
      integer(k1) :: w
      contains
         procedure :: sub => subgen3
         generic :: operator(-) => sub
   end type

   contains

   class(base(4)) function sub ( a, b )
      class(base(4)), intent(in) :: a, b
      allocatable :: sub
      allocate ( base(4) :: sub )
      sub%x = a%x -b%x
      sub%y = a%y -b%y

   end function

    class(base(4)) function subchild ( a, b )
      class(base(4)), intent(in)  ::  b
      class(child(4)), intent(in) ::  a
      allocatable :: subchild

      allocate ( child(4) :: subchild )
      select type ( subchild )
         class is ( child(4) )
            select type ( b )
               class is ( child(4) )
                  subchild%base = a%base - b%base
                  subchild%z = a%z - b%z
            end select
      end select

   end function

    class(base(4)) function subgen3 ( a, b )
      class(base(4)), intent(in) ::  b
      class(gen3(4)), intent(in) ::  a
      allocatable :: subgen3

      select type ( b )
         class is ( gen3(4) )
            allocate ( subgen3, source = gen3(4) ( base = a%base - b%base, z = a%z - b%z , w = a%w - b%w ) )
      end select
   end function

end module

program genericOperatorScalar006
  use m

   class(base(4)), pointer :: b1, b2, b3

   allocate ( b1, source = base(4)( 20, 30 ) )
   allocate ( b2, source = base(4)( 30, 50 ) )

   allocate ( b3, source = b2 - b1 )
   print *, b3%x, b3%y

   nullify ( b3, b2, b1 )
   allocate ( b1, source = child(4)(100, 200, 300) )
   allocate ( b2, source = child(4)(50, 100, 150 ) )
   allocate ( b3, source = b1 - b2 )

   select type ( b3 )
      type is ( child(4) )
         print *, b3%x, b3%y, b3%z
   end select

   nullify ( b3, b2, b1 )
   allocate ( b1, source = gen3(4)(10,20,30,40) )
   allocate ( b2, source = gen3(4)(5,10,15,20) )
   allocate ( b3, source = b1 - b2 )

   select type ( b3 )
      type is ( gen3(4) )
         print *, b3%x, b3%y, b3%z, b3%w
   end select

end program
