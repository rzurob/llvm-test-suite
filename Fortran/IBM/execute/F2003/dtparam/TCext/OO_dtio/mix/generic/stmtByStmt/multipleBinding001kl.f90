!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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
!*                                             - define all and some of the four dtio-generic-spec with generic-binding
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

   type :: base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         procedure, pass :: fwrite
         generic :: write(formatted) => fwrite
         procedure, pass :: fread
         generic :: read(formatted) => fread
         procedure, pass :: uwrite => unformattedwrite
         procedure, pass :: uread  => unformattedread
         generic :: write(unformatted) => uwrite
         generic :: read(unformatted) => uread
   end type

   contains

      subroutine fwrite (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'fwrite'

      end subroutine

      subroutine fread (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'fread'
      end subroutine

      subroutine unformattedread (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'uread'
      end subroutine

      subroutine unformattedwrite (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'uwrite'
      end subroutine

end module

program multipleBinding001kl
   use m

   class(base(:)), allocatable :: b1, b2, b3 ! tcx: (:)

   integer :: stat
   character(200) :: msg
   namelist /nml/ b2

   allocate ( b1, source = base(3)('IBM') ) ! tcx: (3)
   allocate ( b2, source = base(3)('FTN') ) ! tcx: (3)
   allocate (base(3):: b3 ) ! tcx: base(3)

   open ( 1, file = 'multipleBinding001kl.1', form='formatted', access='sequential' )
   open ( 2, file = 'multipleBinding001kl.2', form='unformatted', access='direct', recl = 5 )

   ! Formatted I/O

   write ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'fwrite' ) ) error stop 101_4

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

   write ( 2, iostat = stat, iomsg = msg, rec = 5 )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'uwrite' ) ) error stop 7_4

   write ( 2, iostat = stat, iomsg = msg, rec = 10 )  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'uwrite' ) ) error stop 8_4

   read ( 2, iostat = stat, iomsg = msg, rec = 5 )    b2
   if ( ( stat /= 0 ) .or. ( msg /= 'uread' ) )  error stop 9_4
   if ( b2%c /= 'IBM' ) error stop 10_4

   b2%c = ''

   read ( 2, iostat = stat, iomsg = msg, rec = 10 )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'uread' ) )  error stop 11_4
   if ( b1%c /= 'FTN' ) error stop 12_4
   close (1, status = 'delete' )
   close (2, status = 'delete' )
end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
