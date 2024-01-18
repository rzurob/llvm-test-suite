!*  ===================================================================
!*
!*  TEST CASE NAME             : array108kl
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array unlimited polymorphic entity within select type (read)
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

   type base (lb)
      integer, len :: lb
      character(lb) :: c = 'xxx'
   end type

   type, extends(base) :: child (kc)
   integer, kind :: kc
      integer(kc) :: i = -99
   end type

   type, extends(child) :: gen3 (kg)
   integer, kind :: kg
      integer(kg) :: j = -999
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: idx
   character(15) :: rbuffer(16) = ''

end module

program array108kl
use m

   class(*), target, allocatable :: u1(:), u2(:,:)
   class(*), pointer             :: u3(:), u4(:,:)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(4(DT'_u1'(1)))"

   open (1, file = 'array108kl.1', form='formatted', access='sequential' )

   allocate( gen3(3,4,4) :: u1(4) )
   allocate( child(3,4) :: u2(2,2) )
   u3 => u1(4:1:-1)
   u4 => u2(1:2,1:2)

10 format (DT'_u2-1'(2,3),/, DT'_u2-2'(3,4))
20 format (DT'_u3-1'(5),/,DT'_u3-2'(6),/,DT'_u3-3'(7))

   idx = 1

   select type ( u1 )
      type is ( gen3(*,4,4) )
         read ( 1, fmt, iostat = stat, iomsg = msg )                  u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 1_4
         if ( ( u1(1)%c /= 'abc' ) .or. ( u1(1)%i /= 101 ) .or. ( u1(1)%j /= 111 )  .or. &
              ( u1(2)%c /= 'def' ) .or. ( u1(2)%i /= 102 ) .or. ( u1(2)%j /= 112 )  .or. &
              ( u1(3)%c /= 'ghi' ) .or. ( u1(3)%i /= 103 ) .or. ( u1(3)%j /= 113 )  .or. &
              ( u1(4)%c /= 'jkl' ) .or. ( u1(4)%i /= 104 ) .or. ( u1(4)%j /= 114 )  ) error stop 2_4
   end select

   select type ( u2 )
      class is ( base(*) )
         read ( 1, 10, iostat = stat, iomsg = msg )                   u2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 3_4
         select type ( u2 )
            type is ( child(*,4) )
               if ( ( u2(1,1)%c /= 'ABC' ) .or. ( u2(1,1)%i /= 201 )  .or. &
                    ( u2(2,1)%c /= 'DEF' ) .or. ( u2(2,1)%i /= 202 )  .or. &
                    ( u2(1,2)%c /= 'GHI' ) .or. ( u2(1,2)%i /= 203 )  .or. &
                    ( u2(2,2)%c /= 'JKL' ) .or. ( u2(2,2)%i /= 204 )  ) error stop 4_4
         end select
   end select


   select type ( u3 )
      class is ( child(*,4) )
         read ( 1, 20, iostat = stat, iomsg = msg )                   u3(4:1:-1)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )              error stop 5_4
         select type ( u3 )
            type is ( gen3(*,4,4) )
               if ( ( u3(1)%c /= 'abc' ) .or. ( u3(1)%i /= 301 ) .or. ( u3(1)%j /= 311 )  .or. &
                    ( u3(2)%c /= 'def' ) .or. ( u3(2)%i /= 302 ) .or. ( u3(2)%j /= 312 )  .or. &
                    ( u3(3)%c /= 'ghi' ) .or. ( u3(3)%i /= 303 ) .or. ( u3(3)%j /= 313 )  .or. &
                    ( u3(4)%c /= 'jkl' ) .or. ( u3(4)%i /= 304 ) .or. ( u3(4)%j /= 314 )  ) error stop 6_4
         end select
         select type ( u1 )
            type is ( gen3(*,4,4) )
               if ( ( u1(4)%c /= 'abc' ) .or. ( u1(4)%i /= 301 ) .or. ( u1(4)%j /= 311 )  .or. &
                    ( u1(3)%c /= 'def' ) .or. ( u1(3)%i /= 302 ) .or. ( u1(3)%j /= 312 )  .or. &
                    ( u1(2)%c /= 'ghi' ) .or. ( u1(2)%i /= 303 ) .or. ( u1(2)%j /= 313 )  .or. &
                    ( u1(1)%c /= 'jkl' ) .or. ( u1(1)%i /= 304 ) .or. ( u1(1)%j /= 314 )  ) error stop 7_4
         end select
   end select

   print *, rbuffer(:idx-1)

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, gen3, rbuffer, idx

   class(base(*)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      class is ( base(*) )
         read ( unit, "(/5X,A3)" ) dtv%c
   end select
   select type ( dtv )
      class is ( child(*,4) )
         read ( unit, "(/5X,I3)" ) dtv%i
   end select
   select type ( dtv )
      class is ( gen3(*,4,4) )
         read ( unit, "(/5X,I3)" ) dtv%j
   end select

   iomsg = 'dtioread'

end subroutine
