!*  ===================================================================
!*
!*  TEST CASE NAME             : C461_002kl
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Syntax Check: C461 If generic-spec is not generic-name, each
!*                                                  of its specific bindings shall have a passed-
!*                                                  object dummy argument.
!*
!*                                             - specific binding does not specify any PASS/NOPASS attributes
!*                                               (default is PASS)
!*                               adaptation: exposed length
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

   type ::  base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c
      contains
         procedure :: fread
         procedure :: fwrite
         generic, public  :: write(formatted)  => fwrite
         generic, public  :: read(formatted)   => fread
   end type

   contains

      subroutine fwrite (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit,* ) dtv%c
         iomsg = 'dtiowrite'

      end subroutine

      subroutine fread (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit,*, iomsg = iomsg ) dtv%c
         iomsg = 'dtioread'
      end subroutine

end module

program C461_002kl
   use m
   type(base(3)) :: b1 = base(3)('IBM') ! tcx: (3) ! tcx: (3)
   type(base(:)), allocatable, target :: b2 ! tcx: (:)
   class(base(:)), pointer :: b3 ! tcx: (:)

   character(200) :: msg = ''

   open ( 1, file='C461_002kl.1', form='formatted', access='sequential' )
   write ( 1, "(DT)", iomsg = msg ) b1

   if ( msg /= 'dtiowrite' ) error stop 1_4

   allocate (base(3):: b2 ) ! tcx: base(3)
   b3 => b2

   rewind 1

   read ( 1, *, iostat = i,iomsg = msg )  b3
   if ( ( msg /= 'dtioread' ) .or. ( b3%c /= 'IBM' ) .or. ( b2%c /= 'IBM' ) ) error stop 2_4

   close (1, status ='delete')
end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
! type: nil - added parameters (kbase_1) to invoke with (4) / declare with (4) - 0 changes
