!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Type-bound procedure overriding
!*                               viii)if the overridden binding is PRIVATE, then the overridding binding can be PUBLIC
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module n

   type, abstract :: base
      integer :: id = -999
      contains
         procedure(inf), deferred, pass, private :: setid
   end type

   type, extends(base) :: child
      integer :: rid = -999
      contains
         procedure, pass :: setid
   end type

   interface
      subroutine inf(dtv,j)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine
   end interface

   contains

      subroutine setid(dtv,j)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: j
         dtv%id = j
      end subroutine

end module

program override008
   use n

   type(child) :: c1
   class(child), pointer :: c2
   class(base), allocatable :: b1

   allocate ( c2, source = child(1,2) )
   allocate ( b1, source = child(10, 20 ) )
   
   call c1%setid(12)
   call c2%setid(13)
   
   select type ( b1 )
      type is ( child )
         call b1%setid(100)
         if ( ( b1%id /= 100 ) .or. ( b1%rid /= 20 ) ) error stop 1_4
   end select
   
   if ( ( c1%id /= 12 ) .or. ( c1%rid /= -999 ) ) error stop 2_4
   if ( ( c2%id /= 13 ) .or. ( c2%rid /= 2 ) )    error stop 3_4
   
end program
