! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/deferred/deferred006.f
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
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - Recursive Deferred Binding in base and child type
!*                                    and non-recursive in gen3 type
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

   type, abstract :: base(k1)    ! (4)
      integer, kind            :: k1
      class(base(k1)), pointer :: next => null()
      integer(k1)              :: i
      contains
         procedure(inf), deferred, pass :: print
   end type

   interface
      recursive subroutine inf(dtv)
         import base
         class(base(4)), intent(inout) :: dtv
      end subroutine
   end interface

end module


module n1
   use n, only: base

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: j
      contains
         procedure, pass :: print => printchild
   end type

   type, extends(child) :: gen3(k3)    ! (4,4,4)
      integer, kind :: k3
      integer(k3)   :: k
      contains
         procedure, pass :: print => printgen3
   end type

   contains

      recursive subroutine printchild(dtv)
         class(child(4,4)), intent(inout) :: dtv
         print *, dtv%i, dtv%j
         if ( associated ( dtv%next ) ) then
            call dtv%next%print()
         end if
      end subroutine

      subroutine printgen3(dtv)
         class(gen3(4,4,4)), intent(inout) :: dtv
         print *, dtv%i, dtv%j, dtv%k
      end subroutine

end module

program deferred006
   use n1

   class(base(4)), pointer :: b1
   class(child(4,4)), allocatable :: c1

   allocate ( b1, source = child(4,4)( i=1, j=11 ) )
   allocate ( b1%next, source = gen3(4,4,4)( i=2, j=22, k=222 ) )
   allocate ( b1%next%next, source = child(4,4)( i=3, j=33 ) )
   allocate ( b1%next%next%next, source = child(4,4)( i=4, j=44 ) )
   allocate ( b1%next%next%next%next, source = child(4,4)( i=5, j=55 ) )
   allocate ( b1%next%next%next%next%next, source = child(4,4)( i=6, j=66 ) )

   call b1%print()
   call b1%next%next%print()

   allocate ( c1, source = child(4,4)( i = -1, j = -11 ) )
   c1%next => b1%next%next

   call c1%print()

end program
