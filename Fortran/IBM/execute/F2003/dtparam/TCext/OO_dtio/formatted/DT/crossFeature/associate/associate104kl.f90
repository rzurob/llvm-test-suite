!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate104kl
!*
!*  PROGRAMMER                 : David Forster (derived from associate104 by Robert Ma)
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Associate Constructor: Unlimited Polymorphic Array Entities (read)
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
      real(kb) :: i = -999.0
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      real(kc) :: j = -999.0
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
   character(20) :: rbuffer(10)
   integer(4) :: idx

end module

program associate104kl
use m

   class(*), allocatable  :: u1(:)
   class(*), pointer      :: u2(:,:)

   class(base(4)), allocatable  :: b1(:)
   class(child(4,4)), allocatable, target :: c1(:,:)

   logical :: precision_r4

   open (1, file = 'associate104kl.1', form='formatted', access='stream' )

   allocate ( b1(3) )
   allocate ( c1(2,2) )

   allocate ( u1(3), source = b1 )
   u2 => c1

   idx = 1

   associate ( g => u1 )
      select type ( g )
         class is ( base(4) )
            read ( 1, "(DT'b1-1'(7,2), DT'b1-2'(7,2), DT'b1-3'(7,2))", iostat = stat, iomsg = msg, pos = 102 ) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
            print *, g%i
      end select
   end associate

   select type ( u2 )
      class is ( base(4) )
         associate ( g => u2 )
            read ( 1, "(DT'u2-1'(7,2,8,3),DT'u2-2'(7,2,8,3),DT'u2-3'(7,2,8,3),DT'u2-4'(7,2,8,3))", iostat = stat, iomsg = msg, pos = 42) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
            select type ( g )
               type is ( child(4,4) )
                  print *, g%i
                  print *, g%j
                  print *, c1%i
                  print *, c1%j
            end select
         end associate
   end select

   deallocate ( u1 )

   allocate ( u1(2), source = (/ child(4,4)(201.0, 202.0), child(4,4)(203.0, 204.0 ) /) )
   allocate ( u2(1,1), source = reshape ( source =  (/ base(4) (205.0) /), shape = (/1,1 /) ) )

   associate ( d => u1 )
      select type ( d )
         type is ( child(4,4) )
            read ( 1, "(DT'u1-1'(8,3,9,4), DT'u1-2'(8,3,9,4))", iostat = stat, iomsg = msg, pos = 8 ) d
            if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
            print *, d%i
            print *, d%j            
      end select
   end associate

   select type ( f => u2 )
      class is ( base(4) )
         associate ( g => f )
            read ( 1, "(DT(7,2))", iostat = stat, iomsg = msg, pos = 1 ) g
            if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
            print *, g%i
         end associate
   end select

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) len(iotype), size(v_list), iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base(4) )
         write ( fmt, "(A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),')'
         read ( unit, fmt, iostat = iostat)    dtv%i
      type is ( child(4,4) )
         write ( fmt, "(A2,I1,A1,I1,A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),',F',v_list(3),'.',v_list(4),')'
         read ( unit, fmt, iostat = iostat)    dtv%i, dtv%j
   end select
   iomsg = 'dtioread'

end subroutine
