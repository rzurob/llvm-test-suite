! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueStructrConstrAllocatableComponent001.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with derived allocatable components, pass
!*                                         structure component as actual arg (including parent), try structure constructor being the actual arg
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
   end type

   type base(k2)    ! (4)
      integer, kind                :: k2
      type(inner(k2)), allocatable :: inb
   end type

   type, extends(base) :: child    ! (4)
      type(inner(k2)), allocatable :: inc
   end type

   contains

      subroutine foo(a)
         type(base(4)), value :: a

         print *, 'inside foo', a%inb%i

         call bar(a%inb)

         print *, allocated(a%inb%i)
         deallocate ( a%inb%i )
         print *, allocated(a%inb%i)
         deallocate ( a%inb )
         print *, 'after foo', allocated(a%inb)

      end subroutine

      subroutine bar(a)
         type(inner(4)), value :: a

         print *, 'inside bar', a%i
         deallocate ( a%i )
         print *, 'after bar', allocated(a%i)

      end subroutine

      subroutine boo(a)
         class(base(4)) :: a

         print *, 'inside boo', a%inb%i

         call foo(a)

         print *, 'inside boo 2', a%inb%i

         a%inb = inner(4)(500)

         print *, 'after boo', allocated(a%inb)

      end subroutine

end module

program valueStructrConstrAllocatableComponent001
   use m

   type(base(4)) b1
   type(child(4)) c1

   allocatable :: c1

   call foo ( base(4)(inner(4)(100)) )
   print *,'==='
   call bar ( inner(4)(200) )
   print *,'==='
   call boo ( child(4)(inner(4)(300), inner(4)(400)))
   print *,'==='

end program