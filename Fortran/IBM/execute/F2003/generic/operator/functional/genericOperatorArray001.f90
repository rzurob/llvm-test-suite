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

   type base
      integer :: i
      contains
         procedure :: combine
         generic :: operator(+) => combine
         procedure :: dtiowrite
         generic :: write(formatted) => dtiowrite
   end type

   contains

      class(base) function combine ( a, b )
         allocatable :: combine(:)
         class(base), intent(in) :: a
         class(base), intent(in) :: b(:)

         allocate ( combine(1+size(b)) , source = (/ a, b /) )

      end function

      subroutine dtiowrite (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
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

   class(base), allocatable :: b1(:), b2
   class(base), pointer :: b3(:)

   allocate ( b2, source = base( 10 ) )
   allocate ( b3(2), source = ( base (20) + (/ base(30) /) ) )
   allocate ( b1(1 + size(b3) ), source = b2 + b3 )

   print *, b1
   print *, b3

   deallocate ( b3 )

   allocate ( b3( size(b1) * 2 + 2 ) , source = b2 + (/ b1, b2, b1 /) )

   print *, b3

end program
