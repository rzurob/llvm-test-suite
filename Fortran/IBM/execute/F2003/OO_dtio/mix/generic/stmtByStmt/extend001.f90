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
!*                               Stmt by Stmt: (Pg.60 14-15) An extended type includes all of the type
!*                                                           parameters, all of the components, and the
!*                                                           nonoverridden (4.5.6.2) nonfinal procedure
!*                                                           bindings of its parent type.
!*
!*                                             - Extension type using parent's dtio generic binding (write)
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
         procedure, pass :: write => writebase
         generic :: write(formatted) => write
   end type

   type, extends(base) :: child
      integer :: i
   end type

   type, extends(child) :: gen3
      integer :: j
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base )
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
               iomsg = 'dtiowritebase'
            type is ( child )
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtiowritechild'
            type is ( gen3 )
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
               iomsg = 'dtiowritegen3'
         end select

      end subroutine

end module

program extend001
   use m

   class(base), allocatable :: b1
   class(child), pointer    :: c1
   class(gen3), allocatable :: g1

   type ( child ) :: c2 = child ( 'DEF', 202 )
   type ( gen3 )  :: g2 = gen3  ( 'dEf', 302, 312 )

   namelist /b1nml/ b1

   integer :: stat
   character(200) :: msg

   open ( 101, file = 'extend001.1', form='formatted', access='sequential' )

   allocate ( b1, source = base ('abc') )
   allocate ( c1, source = child('ABC', 201 ) )
   allocate ( g1, source = gen3 ('aBc', 301, 311  ) )

   write ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritebase' ) ) error stop 1_4

   write ( 101, *, iostat = stat, iomsg = msg )         c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) )error stop 2_4

   write ( 101, *, iostat = stat, iomsg = msg )         c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) )error stop 3_4

   write ( 101, *, iostat = stat, iomsg = msg )         g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritegen3' ) ) error stop 4_4

   write ( 101, *, iostat = stat, iomsg = msg )         g2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritegen3' ) ) error stop 5_4

   deallocate ( b1, c1 )

   allocate ( b1, source = child ( 'GHI', 203 ) )
   allocate ( c1, source = gen3 ( 'gHi', 303, 313 ) )

   write ( 101, "(DT)", iostat = stat, iomsg = msg )    b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) )error stop 6_4

   write ( 101, "(DT)", iostat = stat, iomsg = msg )    c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritegen3' ) ) error stop 7_4

   deallocate ( b1 )

   allocate ( b1, source = gen3 ( 'GHI', 304, 314 ) )

   write ( 101, b1nml, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritegen3' ) ) error stop 8_4

end program
