!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Stmt by Stmt: (Pg.199 12-16) For a particular derived type and a particular
!*                                             set of kind type parameter values, there are four possible sets
!*                                             of characteristics for user-defined derived-type input/output
!*                                             procedures; one each for formatted input, formatted output,
!*                                             unformatted input, and unformatted output. The user need not supply
!*                                             all four procedures. The procedures are specified to be used for
!*                                             derived-type input/output by interface blocks (12.3.2.1) or by
!*                                             generic bindings (4.5.4), with a dtio-generic-spec (R1208).
!*
!*                                             - define some with dtio-generic-spec with generic-binding and some with
!*                                               dtio generic interface
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

   type :: base
      character(3) :: c = 'xxx'
      contains

         procedure, pass :: fwrite
         procedure, pass :: uwrite => unformattedwrite

         generic :: write(formatted) => fwrite
         generic :: write(unformatted) => uwrite

   end type

   interface read(formatted)
      subroutine fread (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface read(formatted)

   interface read(unformatted)
      module procedure unformattedread
   end interface read(unformatted)

   contains

      subroutine fwrite (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'fwrite'

      end subroutine

      subroutine unformattedwrite (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'uwrite'
      end subroutine

      subroutine unformattedread (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'uread'
      end subroutine

end module

program multipleBinding002
   use m

   class(base), allocatable :: b1, b2, b3

   integer :: stat
   character(200) :: msg
   namelist /nml/ b2

   allocate ( b1, source = base('IBM') )
   allocate ( b2, source = base('FTN') )
   allocate ( b3 )

   open ( 1, file = 'multipleBinding002.1', form='formatted', access='sequential' )
   open ( 2, file = 'multipleBinding002.2', form='unformatted', access='stream' )

   ! Formatted I/O

   write ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'fwrite' ) ) error stop 1_4

   write ( 1, nml, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'fwrite' ) ) error stop 2_4

   rewind 1

   read ( 1, *, iostat = stat, iomsg = msg )  b3
   if ( ( stat /= 0 ) .or. ( msg /= 'fread' ) )  error stop 3_4
   if ( b3%c /= 'IBM' ) error stop 4_4

   b2%c = ''

   read ( 1, nml, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'fread' ) )  error stop 5_4
   if ( b2%c /= 'FTN' ) error stop 6_4

   ! Unformatted I/O

   write ( 2, iostat = stat, iomsg = msg, pos = 5 )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'uwrite' ) ) error stop 7_4

   write ( 2, iostat = stat, iomsg = msg, pos = 10 )  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'uwrite' ) ) error stop 8_4

   read ( 2, iostat = stat, iomsg = msg, pos = 5 )    b2
   if ( ( stat /= 0 ) .or. ( msg /= 'uread' ) )  error stop 9_4
   if ( b2%c /= 'IBM' ) error stop 10_4

   b2%c = ''

   read ( 2, iostat = stat, iomsg = msg, pos = 10 )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'uread' ) )  error stop 11_4
   if ( b1%c /= 'FTN' ) error stop 12_4

   close ( 1, status='delete' )
   close ( 2, status='delete' )

end program

subroutine fread (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
   iomsg = 'fread'
end subroutine