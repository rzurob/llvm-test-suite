!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: with deferred binding in type components
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

   type, abstract :: inner
      integer(4) :: i
      contains
         procedure(itf), deferred, pass :: assignmt
         generic, private :: assignment(=) => assignmt
   end type

   type, extends(inner) :: cinner
      integer(4) :: j
      contains
         procedure, pass :: assignmt => cassignmt
   end type

   interface
      subroutine itf ( a, b )
         import inner
         class(inner), intent(out) :: a
         class(inner), intent(in)  :: b
      end subroutine
   end interface

   type base
      class(inner), allocatable :: in1
      class(inner), allocatable :: in2
      contains
         procedure, pass :: bassignmt
         generic :: assignment(=) => bassignmt
   end type

   contains

   subroutine cassignmt ( a, b )
      class(cinner), intent(out) :: a
      class(inner), intent(in)   :: b

      a%i = b%i

      select type ( b )
         type is ( cinner )
            a%j = b%j
         class default
            error stop 1_4
      end select

   end subroutine

   subroutine bassignmt ( a, b )
      class(base), intent(out) :: a
      class(base), intent(in)   :: b

      if ( .not. allocated(a%in1) ) allocate ( cinner:: a%in1 )
      if ( .not. allocated(a%in2) ) allocate ( cinner:: a%in2 )

      a%in1 = b%in1
      a%in2 = b%in2

   end subroutine

end module

program genericAssignmentDeferred004
   use m

   class(base), allocatable, target :: b1
   type(base), pointer :: b2
   type(base) :: b3

   allocate ( b1, source = base ( cinner(10,11), cinner(20, 21) ) )
   b2 => b1

   b3 = b2

   select type ( g => b3%in1 )
      type is ( cinner )
         print *, g
   end select
   select type ( g => b3%in2 )
      type is ( cinner )
         print *, g
   end select

   b3%in1%i = -999
   b3%in2%i = -999

   b2 = b3

   select type ( g => b2%in1 )
      type is ( cinner )
         print *, g
   end select

   select type ( g => b2%in2 )
      type is ( cinner )
         print *, g
   end select

   select type ( g => b1%in1 )
      type is ( cinner )
         print *, g
   end select

   select type ( g => b1%in2 )
      type is ( cinner )
         print *, g
   end select

end program
