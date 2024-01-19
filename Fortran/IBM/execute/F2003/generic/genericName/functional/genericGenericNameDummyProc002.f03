!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb dummy arg has a dummy procedure specified with an generic interface
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

   type, extends(base) :: child
      integer :: j = -999
      contains
         procedure, pass :: setbase => setchild
         procedure, pass :: setsumbase => setsumchild
         procedure, pass :: setsumchildchild
         generic :: set => setsumchildchild
   end type

   abstract interface
      class(base) function abaseinterface(i,j)
         import base
         allocatable :: abaseinterface
         integer, intent(in) :: i
         integer, intent(in), optional :: j
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
         procedure(abaseinterface) :: b
         integer, intent(in) :: c, d

         select type ( a )
            type is ( base )
               a = b(c+d)
         end select

         print *, 'setsumbase'

      end subroutine

      subroutine setchild (a,b,c)
         class(child), intent(inout) :: a
         procedure(abaseinterface) :: b
         integer, intent(in) :: c

         select type ( a )
            type is ( child )
               select type ( g => b(c,c) )
                  type is ( child )
                     a = g
               end select
         end select

         print *, 'setchild'

      end subroutine

      subroutine setsumchild (a,b,c,d)
         class(child), intent(inout) :: a
         procedure(abaseinterface) :: b
         integer, intent(in) :: c, d

         select type ( a )
            type is ( child )
               select type ( g => b(c+d,c+d) )
                  type is ( child )
                     a = g
               end select
         end select

         print *, 'setsumchild'

      end subroutine

      subroutine setsumchildchild (a,b,c,d,e,f)
         class(child), intent(inout) :: a
         procedure(abaseinterface) :: b
         integer, intent(in) :: c, d, e, f

         select type ( a )
            type is ( child )
               select type ( g => b(c+d,e+f) )
                  type is ( child )
                     a = g
               end select
         end select

         print *, 'setsumchildchild'

      end subroutine

end module

class(base) function abase(i,j)
   use m, only: base, child
   integer, intent(in) :: i, j
   allocatable :: abase
   optional :: j

   if ( present(j) ) then
      allocate ( abase, source = child(i,j) )
   else
      allocate ( abase, source = base(i) )
   end if

   print *, 'abase'

end function

class(base) function anegbase(i, j)
   use m, only: base, child
   integer, intent(in) :: i
   optional :: j
   allocatable :: anegbase

   if ( present(j) ) then
      allocate ( anegbase, source = child(-1*i,-1*j) )
   else
      allocate ( anegbase, source = base(-1*i) )
   end if

   print *, 'anegbase'

end function

program genericGenericNameDummyProc002
   use m

   class(base), allocatable :: b1
   class(child), pointer :: c1

   procedure(abaseinterface) :: abase, anegbase

   allocate ( b1 )
   call b1%set(abase,10)
   print *, b1%i

   call b1%set(abase,10,20)
   print *, b1%i

   call b1%set(anegbase,10)
   print *, b1%i

   call b1%set(anegbase,10,20)
   print *, b1%i

   allocate ( c1 )

   call c1%set(abase,10)
   print *, c1%i, c1%j

   call c1%set(abase,10,20)
   print *, c1%i, c1%j

   call c1%set(abase,10,20,30,40)
   print *, c1%i, c1%j

   call c1%set(anegbase,10)
   print *, c1%i, c1%j

   call c1%set(anegbase,10,20)
   print *, c1%i, c1%j

   call c1%set(anegbase,10,20,30,40)
   print *, c1%i, c1%j

   deallocate ( b1 )
   allocate ( child :: b1 )

   call b1%set(abase,100)
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   call b1%set(abase,100,200)
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   select type ( b1 )
      type is ( child )
         call b1%set(abase,100,200,300,400)
         print *, b1%i, b1%j
   end select

   call b1%set(anegbase,100)
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   call b1%set(anegbase,100,200)
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   select type ( b1 )
      type is ( child )
         call b1%set(anegbase,100,200,300,400)
         print *, b1%i, b1%j
   end select

end program
