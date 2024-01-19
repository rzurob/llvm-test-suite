!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - finalization procedure containing value attribute dummy arg
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
      integer, pointer :: i
   end type

   type base
      type(inner), allocatable :: inb
      contains
         final :: finalbase
   end type

   integer, target :: i

   contains

      subroutine finalbase ( a )
         type(base), value :: a

         print *, 'finalbase'
         if ( associated (a%inb%i) ) then
            print *, associated ( a%inb%i,i )
            print *, a%inb%i
            a%inb%i = -999

         end if
         print *, 'done', allocated(a%inb)

      end subroutine

end module

program valueAllocatableComponentFinal001
   use m

   type(base) :: b1
   type(base), allocatable :: b2
   i = 10

   print *, 'finalize b1, and base():'
   b1 = base(inner(i))
   print *, i

   i = 100

   print *, 'finalize base():'
   allocate ( b2, source = base(inner(i)) )
   print *,i

   i = 1000
   print *, 'finalize b2:'
   deallocate ( b2 )
   print *,i

end program
