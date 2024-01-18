!*  ===================================================================
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: polymorphic scalar dummy argument (read)
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

   type base (kb)
      integer, kind :: kb
      integer(kb) :: i = -999
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j = -999
   end type

   type, extends(child) :: gen3 (kg)
      integer, kind :: kg
      integer(kg) :: k = -999
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg
   character(20) :: rbuffer(13)
   integer :: idx

   contains

      subroutine foo ( dtv )
         class(base(4)), intent(inout) :: dtv
         character(17) :: fmt = "(DT'_foo'(5,6,7))"

         read (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(child(4,4)), intent(inout) :: dtv
      10 format (DT'_bar'(8,9,10))

         read (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4

      end subroutine

end module

program dummyArg102kl
use m

   type(base(4)), allocatable  :: b1
   type(base(4))               :: b2 = base(4) ()
   class(base(4)), pointer     :: b3

   type(child(4,4)), pointer      :: c1
   type(child(4,4))               :: c2 = child(4,4) ()
   class(child(4,4)), allocatable :: c3

   type(gen3(4,4,4)) :: g1 = gen3(4,4,4)()
   class(gen3(4,4,4)), pointer :: g2

   open (1, file = 'dummyArg102kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)() )
   allocate ( b3, source = gen3(4,4,4)() )

   allocate ( c1, source = child(4,4)() )
   allocate ( c3, source = gen3(4,4,4) () )
   allocate ( g2, source = gen3(4,4,4) () )

   idx = 1

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )

   call foo ( c1 )
   call foo ( c2 )
   call foo ( c3 )

   call foo ( g1 )
   call foo ( g2 )

   if ( b1%i /= 101 ) error stop 3_4
   if ( b2%i /= 102 ) error stop 4_4
   select type ( b3 )
      type is ( gen3(4,4,4) )
         if ( ( b3%i /= 103 ) .or. ( b3%j /= 113 ) .or. ( b3%k /= 123 ) ) error stop 5_4
   end select

   if ( ( c1%i /= 201 ) .or. ( c1%j /= 211 ) ) error stop 6_4
   if ( ( c2%i /= 202 ) .or. ( c2%j /= 212 ) ) error stop 7_4
   select type ( c3 )
      type is ( gen3(4,4,4) )
         if ( ( c3%i /= 203 ) .or. ( c3%j /= 213 ) .or. ( c3%k /= 223 ) ) error stop 8_4
   end select

   if ( ( g1%i /= 301 ) .or. ( g1%j /= 311 ) .or. ( g1%k /= 321 ) )       error stop 9_4
   if ( ( g2%i /= 302 ) .or. ( g2%j /= 312 ) .or. ( g2%k /= 322 ) )       error stop 10_4

   call bar ( c1 )
   call bar ( c2 )
   call bar ( c3 )

   call bar ( g1 )
   call bar ( g2 )

   if ( ( c1%i /= 204 ) .or. ( c1%j /= 214 ) ) error stop 11_4
   if ( ( c2%i /= 205 ) .or. ( c2%j /= 215 ) ) error stop 12_4
   select type ( c3 )
      type is ( gen3(4,4,4) )
         if ( ( c3%i /= 206 ) .or. ( c3%j /= 216 ) .or. ( c3%k /= 226 ) ) error stop 13_4
   end select

   if ( ( g1%i /= 303 ) .or. ( g1%j /= 313 ) .or. ( g1%k /= 323 ) )       error stop 14_4
   if ( ( g2%i /= 304 ) .or. ( g2%j /= 314 ) .or. ( g2%k /= 324 ) )       error stop 15_4

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, gen3, idx, rbuffer

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base(4) )
         write ( fmt, * ) '(I', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child(4,4) )
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
      type is ( gen3(4,4,4) )
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),', I',v_list(3),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%j, dtv%k
   end select
   iomsg = 'dtioread'

end subroutine
