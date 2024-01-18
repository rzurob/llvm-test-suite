! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/generic/genericName/functional/genericGenericNameDummyProc002.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure, pass :: setbase
         procedure, pass :: setsumbase
         generic :: set => setbase, setsumbase
   end type

   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j = -999
      contains
         procedure, pass :: setbase => setchild
         procedure, pass :: setsumbase => setsumchild
         procedure, pass :: setsumchildchild
         generic :: set => setsumchildchild
   end type

   abstract interface
      class(base(:,4)) function abaseinterface(i,j)
         import base
         allocatable :: abaseinterface
         integer, intent(in) :: i
         integer, intent(in), optional :: j
      end function
   end interface

   contains

      subroutine setbase (a,b,c)
         class(base(*,4)), intent(inout) :: a
         procedure(abaseinterface) :: b
         integer, intent(in) :: c

         select type ( a )
            type is ( base(*,4) )
               a = b(c)
         end select

         print *, 'setbase'

      end subroutine

      subroutine setsumbase (a,b,c,d)
         class(base(*,4)), intent(inout) :: a
         procedure(abaseinterface) :: b
         integer, intent(in) :: c, d

         select type ( a )
            type is ( base(*,4) )
               a = b(c+d)
         end select

         print *, 'setsumbase'

      end subroutine

      subroutine setchild (a,b,c)
         class(child(*,4,*,4)), intent(inout) :: a
         procedure(abaseinterface) :: b
         integer, intent(in) :: c

         select type ( a )
            type is ( child(*,4,*,4) )
               select type ( g => b(c,c) )
                  type is ( child(*,4,*,4) )
                     a = g
               end select
         end select

         print *, 'setchild'

      end subroutine

      subroutine setsumchild (a,b,c,d)
         class(child(*,4,*,4)), intent(inout) :: a
         procedure(abaseinterface) :: b
         integer, intent(in) :: c, d

         select type ( a )
            type is ( child(*,4,*,4) )
               select type ( g => b(c+d,c+d) )
                  type is ( child(*,4,*,4) )
                     a = g
               end select
         end select

         print *, 'setsumchild'

      end subroutine

      subroutine setsumchildchild (a,b,c,d,e,f)
         class(child(*,4,*,4)), intent(inout) :: a
         procedure(abaseinterface) :: b
         integer, intent(in) :: c, d, e, f

         select type ( a )
            type is ( child(*,4,*,4) )
               select type ( g => b(c+d,e+f) )
                  type is ( child(*,4,*,4) )
                     a = g
               end select
         end select

         print *, 'setsumchildchild'

      end subroutine

end module

class(base(:,4)) function abase(i,j)
   use m, only: base, child
   integer, intent(in) :: i, j
   allocatable :: abase
   optional :: j

   if ( present(j) ) then
      allocate ( abase, source = child(20,4,20,4)(i,j) )
   else
      allocate ( abase, source = base(20,4)(i) )
   end if

   print *, 'abase'

end function

class(base(:,4)) function anegbase(i, j)
   use m, only: base, child
   integer, intent(in) :: i
   optional :: j
   allocatable :: anegbase

   if ( present(j) ) then
      allocate ( anegbase, source = child(20,4,20,4)(-1*i,-1*j) )
   else
      allocate ( anegbase, source = base(20,4)(-1*i) )
   end if

   print *, 'anegbase'

end function

program genericGenericNameDummyProc002
   use m

   class(base(:,4)), allocatable :: b1
   class(child(:,4,:,4)), pointer :: c1

   procedure(abaseinterface) :: abase, anegbase

   allocate ( base(20,4)::b1 )
   call b1%set(abase,10)
   print *, b1%i

   call b1%set(abase,10,20)
   print *, b1%i

   call b1%set(anegbase,10)
   print *, b1%i

   call b1%set(anegbase,10,20)
   print *, b1%i

   allocate ( child(20,4,20,4)::c1 )

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
   allocate ( child(20,4,20,4) :: b1 )

   call b1%set(abase,100)
   select type ( b1 )
      type is ( child(*,4,*,4) )
         print *, b1%i, b1%j
   end select

   call b1%set(abase,100,200)
   select type ( b1 )
      type is ( child(*,4,*,4) )
         print *, b1%i, b1%j
   end select
   
   select type ( b1 )
      type is ( child(*,4,*,4) )
         call b1%set(abase,100,200,300,400)
         print *, b1%i, b1%j
   end select

   call b1%set(anegbase,100)
   select type ( b1 )
      type is ( child(*,4,*,4) )
         print *, b1%i, b1%j
   end select

   call b1%set(anegbase,100,200)
   select type ( b1 )
      type is ( child(*,4,*,4) )
         print *, b1%i, b1%j
   end select

   select type ( b1 )
      type is ( child(*,4,*,4) )
         call b1%set(anegbase,100,200,300,400)
         print *, b1%i, b1%j
   end select

end program
