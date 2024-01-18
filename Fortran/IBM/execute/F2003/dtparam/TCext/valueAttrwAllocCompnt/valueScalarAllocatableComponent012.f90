! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueScalarAllocatableComponent012.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

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
!*                                 - type: derived type with intrinsic allocatable components, pass
!*                                         structure component as actual arg (including parent)
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

program valueScalarAllocatableComponent012
   use m

   type(base(4)) b1
   type(child(4)) c1

   allocatable :: c1

   b1 = base(4)(inner(4)(100))

   call foo ( b1 )

   print *, '=========='
   print *, 'main b1', allocated ( b1%inb%i ), allocated ( b1%inb ), b1%inb%i
   print *, '=========='

   allocate ( c1 )

   c1 = child(4)( inner(4)(200), inner(4)(300) )

   call foo ( c1%base )
   print *, '=========='
   print *, 'main c1', allocated ( c1%inb%i ), allocated ( c1%inb ), c1%inb%i
   print *, '=========='

   call bar ( c1%base%inb )
   print *, '=========='
   print *, 'main c1%inb', allocated ( c1%inb%i ), allocated ( c1%inb ), c1%inb%i
   print *, '=========='

   call bar ( c1%inc )
   print *, '=========='
   print *, 'main c1%inc', allocated ( c1%inc%i ), allocated ( c1%inc ), c1%inc%i
   print *, '=========='

   call boo ( c1 )
   print *, '=========='
   print *, 'main c1', allocated ( c1%inb%i ), allocated ( c1%inb ), c1%inb%i
   print *, '=========='
   
end program
