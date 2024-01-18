!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: polymorphic elemental assignment for type component as well as type
!*                                           with child type containing overridding tb
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

   type inner
      integer :: i
      contains
         procedure, pass :: iassgn
         generic :: assignment(=) => iassgn
   end type

   type, extends(inner) :: cinner
      integer :: j
      contains
         procedure, pass :: iassgn => ciassgn
   end type

   type base
      class(inner), allocatable :: in(:)
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      elemental subroutine ciassgn ( a, b )
         class(cinner), intent(out) :: a
         class(inner), intent(in) :: b

         a%i = b%i + 1

         select type ( b )
            type is ( cinner )
               a%j = b%j + 1
         end select

      end subroutine

      elemental subroutine iassgn ( a, b )
         class(inner), intent(out) :: a
         class(inner), intent(in) :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         type(base), intent(in) :: b

         if ( allocated(b%in) ) then
            if ( .not. allocated(a%in) ) allocate ( a%in(size(b%in)), source = b%in )
            a%in = b%in
         end if


      end subroutine

end module

program genericAssignmentElemental006
   use m

   type(base) :: b1
   type(base) :: b2(3)
   type(base), allocatable :: b3(:)

   b1 = base((/inner(100)/))
   print *, b1%in%i

   b2 = base((/ cinner(200,300) /))

   select type ( g=> b2(1)%in )
      type is (cinner)
         print *, g%i, g%j
   end select

   select type ( g=> b2(2)%in )
      type is (cinner)
         print *, g%i, g%j
   end select

   select type ( g=> b2(3)%in )
      type is (cinner)
         print *, g%i, g%j
   end select

   allocate ( b3(5) )
   b3 = base((/cinner(300,400), cinner(500,600)/))

   select type ( g=> b3(1)%in )
      type is (cinner)
         print *, g%i, g%j
   end select
   select type ( g=> b3(2)%in )
      type is (cinner)
         print *, g%i, g%j
   end select
   select type ( g=> b3(3)%in )
      type is (cinner)
         print *, g%i, g%j
   end select
   select type ( g=> b3(4)%in )
      type is (cinner)
         print *, g%i, g%j
   end select
   select type ( g=> b3(5)%in )
      type is (cinner)
         print *, g%i, g%j
   end select

   b2 = (/ base((/cinner(2000, 2001)/)), base((/cinner(2002, 2003), cinner(2004, 2005)/)), base( (/ inner(2006),inner(2007),inner(2008) /) ) /)

   select type ( g=> b2(1)%in )
      type is (cinner)
         print *, g%i, g%j
   end select

   select type ( g=> b2(2)%in )
      type is (cinner)
         print *, g%i, g%j
   end select

   select type ( g=> b2(3)%in )
      type is (inner)
         print *, g%i
   end select

   b3 = (/ b2, base( (/inner(2004), inner(2005), inner(2006)/) ), base( (/cinner(2007,207), cinner(2008,208), cinner(2009,209)/) ) /)

   select type ( g=> b3(1)%in )
      type is (cinner)
         print *, g%i, g%j
   end select
   select type ( g=> b3(2)%in )
      type is (cinner)
         print *, g%i, g%j
   end select
   select type ( g=> b3(3)%in )
      type is (inner)
         print *, g%i
   end select
   select type ( g=> b3(4)%in )
      type is (inner)
         print *, g%i
   end select
   select type ( g=> b3(5)%in )
      type is (cinner)
         print *, g%i, g%j
   end select

end program
