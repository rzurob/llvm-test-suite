! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorArray001.f
! opt variations: -qnol -qnodeferredlp

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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: operators with arrays
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
      integer(k1)   :: i
      contains
         procedure :: combine
         generic :: operator(+) => combine
         procedure :: dtiowrite
         generic :: write(formatted) => dtiowrite
   end type

   contains

      class(base(:,4)) function combine ( a, b )
         allocatable :: combine(:)
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in) :: b(:)

         allocate ( combine(1+size(b)) , source = (/ a, b /) )

      end function

      subroutine dtiowrite (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*,4)), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( unit, *, iostat = iostat, iomsg = iomsg ) dtv%i

      end subroutine


end module

program genericOperatorArray001
   use m

   class(base(:,4)), allocatable :: b1(:), b2
   class(base(:,4)), pointer :: b3(:)

   allocate ( b2, source = base(20,4)( 10 ) )
   allocate ( b3(2), source = ( base(20,4) (20) + (/ base(20,4)(30) /) ) )
   allocate ( b1(1 + size(b3) ), source = b2 + b3 )

   print *, b1
   print *, b3

   deallocate ( b3 )

   allocate ( b3( size(b1) * 2 + 2 ) , source = b2 + (/ b1, b2, b1 /) )

   print *, b3

end program
