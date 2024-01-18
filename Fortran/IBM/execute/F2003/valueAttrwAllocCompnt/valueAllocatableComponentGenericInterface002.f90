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
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - generic interface as structure constructor
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
      integer, allocatable :: i
      contains
         procedure :: geti
         procedure :: seti
   end type

   type base
      type(inner), allocatable :: inb
   end type

   interface base
      module procedure basenoarg
   end interface

   interface inner
      module procedure innernoarg
   end interface

   contains

      subroutine foo(a)
         type(base), value :: a

         print *, 'inside foo', a%inb%geti()

         call a%inb%seti( -200000 )

         print *, 'after foo', a%inb%geti()

      end subroutine

      integer function geti (a)
         class(inner), intent(in) :: a

         if ( allocated(a%i) )  then
            geti = a%i
         else
            geti = 0
         end if
      end function

      subroutine seti (a, i)
         class(inner), intent(inout) :: a
         integer, intent(in) :: i

         if ( allocated(a%i) )  then
            a%i = i
         else
            allocate (a%i, source = i )
         end if
      end subroutine

      type(base) function basenoarg()
         allocate ( basenoarg%inb, source = inner() )
      end function

      type(inner) function innernoarg()
         allocate ( innernoarg%i, source = -999 )
      end function

end module

program valueAllocatableComponentGenericInterface002
   use m

   type(base) :: b1

   call foo( base() )
   call foo(base(inner(10)))
   call foo(base(inner()))

   b1 = base(inner(20))
   call foo ( b1 )

end program
