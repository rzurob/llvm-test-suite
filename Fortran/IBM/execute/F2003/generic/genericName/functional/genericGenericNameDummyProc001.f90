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
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: generic tb dummy arg has a dummy procedure
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

   type base
      integer :: i = -999
      contains
         procedure, pass :: setbase
         procedure, pass :: setsumbase
         generic :: set => setbase, setsumbase
   end type

   abstract interface
      type(base) function abaseinterface(i)
         import base
         integer, intent(in) :: i
      end function
   end interface

   contains

      subroutine setbase (a,b,c)
         class(base), intent(inout) :: a
         procedure(abaseinterface) :: b
         integer, intent(in) :: c

         select type ( a )
            type is ( base )
               a = b(c)
         end select

         print *, 'setbase'

      end subroutine

      subroutine setsumbase (a,b,c,d)
         class(base), intent(inout) :: a
         procedure(type(base)) :: b
         integer, intent(in) :: c, d

         select type ( a )
            type is ( base )
               a = b(c+d)
         end select

         print *, 'setsumbase'

      end subroutine

end module

type(base) function abase(i)
   use m, only: base
   integer, intent(in) :: i

   abase = base(i)

end function

type(base) function anegbase(i)
   use m, only: base
   integer, intent(in) :: i

   anegbase = base(-1*i)

end function

program genericGenericNameDummyProc001
   use m

   type(base) :: b1, b2
   allocatable :: b2

   procedure(abaseinterface) :: abase
   procedure(type(base)) :: anegbase

   call b1%set(abase,10)
   print *, b1%i

   allocate ( b2 )

   call b2%set(anegbase,-20)
   print *, b2%i

   call b1%set(anegbase,-100,-200)
   print *, b1%i

   call b2%set(abase,300,400)
   print *, b2%i

end program
