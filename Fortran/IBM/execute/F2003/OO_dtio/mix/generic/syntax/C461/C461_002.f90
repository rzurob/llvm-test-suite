!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Syntax Check: C461 If generic-spec is not generic-name, each
!*                                                  of its specific bindings shall have a passed-
!*                                                  object dummy argument.
!*
!*                                             - specific binding does not specify any PASS/NOPASS attributes
!*                                               (default is PASS)
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

   type ::  base
      character(3) :: c
      contains
         procedure :: fread
         procedure :: fwrite
         generic, public  :: write(formatted)  => fwrite
         generic, public  :: read(formatted)   => fread
   end type

   contains

      subroutine fwrite (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit,* ) dtv%c
         iomsg = 'dtiowrite'

      end subroutine

      subroutine fread (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit,*, iomsg = iomsg ) dtv%c
         iomsg = 'dtioread'
      end subroutine

end module

program C461_002
   use m
   type(base) :: b1 = base('IBM')
   type(base), allocatable, target :: b2
   class(base), pointer :: b3

   character(200) :: msg = ''

   open ( 1, file='C461_002.1', form='formatted', access='sequential' )
   write ( 1, "(DT)", iomsg = msg ) b1

   if ( msg /= 'dtiowrite' ) error stop 1_4

   allocate ( b2 )
   b3 => b2

   rewind 1

   read ( 1, *, iostat = i,iomsg = msg )  b3
   if ( ( msg /= 'dtioread' ) .or. ( b3%c /= 'IBM' ) .or. ( b2%c /= 'IBM' ) ) error stop 2_4

   close (1, status ='delete')
end program
