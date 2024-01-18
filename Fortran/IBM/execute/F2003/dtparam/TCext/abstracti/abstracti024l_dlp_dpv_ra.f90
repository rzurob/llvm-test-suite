! GM DTP extension using:
! ftcx_dtp -ql -qdefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/abstracti/functional/abstracti024.f

!*  ===================================================================
!*
!*  TEST CASE NAME             : abstracti024l_dlp_dpv_ra
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-30 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*  - Recursive Deferred Binding in base and child type
!*  and non-recursive in gen3 type
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module n

   type, abstract :: base(l1,k1)    ! (20,4)
      integer, kind              :: k1
      integer, len               :: l1
      class(base(:,k1)), pointer :: next => null()
      integer(k1)                :: i
      contains
         procedure(inf), deferred, pass :: print
   end type

   abstract interface
      recursive subroutine inf(dtv)
         import base
         class(base(*,4)), intent(inout) :: dtv
      end subroutine
   end interface

end module


module n1
   use n, only: base

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
      contains
         procedure, pass :: print => printchild
   end type

   type, extends(child) :: gen3    ! (20,4)
      integer(k1) :: k
      contains
         procedure, pass :: print => printgen3
   end type

   contains

      recursive subroutine printchild(dtv)
         class(child(*,4)), intent(inout) :: dtv
         print *, dtv%i, dtv%j
         if ( associated ( dtv%next ) ) then
            call dtv%next%print()
         end if
      end subroutine

      subroutine printgen3(dtv)
         class(gen3(*,4)), intent(inout) :: dtv
         print *, dtv%i, dtv%j, dtv%k
      end subroutine

end module

program abstracti024l_dlp_dpv_ra
   use n1

   class(base(:,4)), pointer :: b1
   class(child(:,4)), allocatable :: c1

   allocate ( b1, source = child(20,4)( i=1, j=11 ) )
   allocate ( b1%next, source = gen3(20,4)( i=2, j=22, k=222 ) )
   allocate ( b1%next%next, source = child(20,4)( i=3, j=33 ) )
   allocate ( b1%next%next%next, source = child(20,4)( i=4, j=44 ) )
   allocate ( b1%next%next%next%next, source = child(20,4)( i=5, j=55 ) )
   allocate ( b1%next%next%next%next%next, source = child(20,4)( i=6, j=66 ) )

   call b1%print()
   call b1%next%next%print()

   allocate ( c1, source = child(20,4)( i = -1, j = -11 ) )
   c1%next => b1%next%next

   call c1%print()

end program abstracti024l_dlp_dpv_ra
