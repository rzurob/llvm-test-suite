!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with derived allocatable components, pass
!*                                         structure component as actual arg (including parent), testing associate name being as actual arg
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
   end type

   type base
      type(inner), allocatable :: inb
   end type

   type, extends(base) :: child
      type(inner), allocatable :: inc
   end type

   contains

      subroutine foo(a)
         type(base), value :: a

         print *, 'inside foo', a%inb%i

         call bar(a%inb)

         print *, 'inside foo1', a%inb%i

         a%inb%i = -999

         print *, 'after foo', allocated(a%inb), a%inb%i

      end subroutine

      subroutine bar(a)
         type(inner), value :: a

         print *, 'inside bar', a%i
         deallocate ( a%i )
         print *, 'after bar', allocated(a%i)

      end subroutine

end module

program valueAssociateNameAllocatableComponent001
   use m

   type(base) b1
   class(base) c1
   allocatable :: c1

   b1 = base(inner(1000))
   allocate ( c1, source = child(inner(2000), inner(3000) ) )

   associate ( g => b1 )
      call foo ( g )
      print *, 'main, b1:', g%inb%i
      associate ( h => c1 )
         call foo ( c1 )
         print *, 'main, c1:',h%inb%i
      end associate
   end associate

   associate ( hh => base(inner(123)) )
      call foo ( hh )
      print *, 'main: struct constr', hh%inb%i
   end associate

   select type ( hh => c1 )
      class is ( base )
         call foo ( hh )
         print *, 'main: c1', hh%inb%i

         associate ( gg => hh )
            call foo ( gg )
            print *, 'main: c1', hh%inb%i
         end associate
   end select

end program
