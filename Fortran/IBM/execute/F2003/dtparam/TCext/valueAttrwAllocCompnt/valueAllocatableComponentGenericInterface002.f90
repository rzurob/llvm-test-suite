! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/valueAllocatableComponentGenericInterface002.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
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

   type inner(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i
      contains
         procedure :: geti
         procedure :: seti
   end type

   type base(k2)    ! (4)
      integer, kind                :: k2
      type(inner(k2)), allocatable :: inb
   end type

   interface base
      module procedure basenoarg
   end interface

   interface inner
      module procedure innernoarg
   end interface

   contains

      subroutine foo(a)
         type(base(4)), value :: a

         print *, 'inside foo', a%inb%geti()

         call a%inb%seti( -200000 )

         print *, 'after foo', a%inb%geti()

      end subroutine

      integer function geti (a)
         class(inner(4)), intent(in) :: a

         if ( allocated(a%i) )  then
            geti = a%i
         else
            geti = 0
         end if
      end function

      subroutine seti (a, i)
         class(inner(4)), intent(inout) :: a
         integer, intent(in) :: i

         if ( allocated(a%i) )  then
            a%i = i
         else
            allocate (a%i, source = i )
         end if
      end subroutine

      type(base(4)) function basenoarg()
         allocate ( basenoarg%inb, source = inner() )
      end function

      type(inner(4)) function innernoarg()
         allocate ( innernoarg%i, source = -999 )
      end function

end module

program valueAllocatableComponentGenericInterface002
   use m

   type(base(4)) :: b1

   call foo( base() )
   call foo(base(4)(inner(4)(10)))
   call foo(base(4)(inner()))

   b1 = base(4)(inner(4)(20))
   call foo ( b1 )

end program
