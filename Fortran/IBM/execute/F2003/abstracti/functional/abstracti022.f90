!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - Private Deferred Binding in base type in 1 module
!*                                    public specific binding in child type in another module
!*
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
      integer :: i = -999
      contains
         procedure(inf), private, deferred, pass :: setid
         procedure, pass :: setidwrapper
   end type

   abstract interface
      subroutine inf(dtv,j)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine
   end interface

   contains

      subroutine setidwrapper ( dtv, i )
         class(base), intent(inout) :: dtv
         integer :: i
         call dtv%setid(i)
      end subroutine

end module

module n1
   use n, only: base

   type, extends(base) :: child
      integer :: j = -999
      contains
         procedure, pass :: setid
   end type

   contains

      subroutine setid ( dtv, j )
         integer, intent(in) :: j
         class(child), intent(inout) :: dtv
         dtv%i = j
         dtv%j = j
      end subroutine

end module

program abstracti022
   use n1

   class(base), pointer              :: b1
   class(child), allocatable, target :: c1

   allocate ( c1, source = child ( 101, 102 ) )
   b1 => c1

   call b1%setidwrapper(201)

   select type ( b1 )
      type is ( child )
         if ( ( c1%i /= 201 ) .or. ( c1%j /= 201 ) .or. ( b1%i /= 201 ) .or. ( b1%j /= 201 ) ) error stop 1_4
   end select

   call c1%setidwrapper(301)

   select type ( b1 )
      type is ( child )
         if ( ( c1%base%i /= 301 ) .or. ( c1%j /= 301 ) .or. ( b1%base%i /= 301 ) .or. ( b1%j /= 301 ) ) error stop 2_4
   end select
   
   call c1%setid(401)

   select type ( b1 )
      type is ( child )
         if ( ( c1%base%i /= 401 ) .or. ( c1%j /= 401 ) .or. ( b1%base%i /= 401 ) .or. ( b1%j /= 401 ) ) error stop 3_4
   end select

end program abstracti022
