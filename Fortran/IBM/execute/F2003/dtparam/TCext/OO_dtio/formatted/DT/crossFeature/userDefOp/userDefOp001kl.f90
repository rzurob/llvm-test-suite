!*  ===================================================================
!*
!*  TEST CASE NAME             : userDefOp001kl
!*
!*  DATE                       : 2007-06-07 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        User Defined Operator: ensure using DT Edit descriptor can invoke DTIO inside
!*                                                               user defined operator procedures
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
      integer(kb) :: i
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j
   end type

   interface operator(+)
      class(base(4)) function myAdd(a,b)
         import base, child
         class(base(4)), intent(in) :: a, b
         allocatable :: myAdd
      end function
   end interface

   interface assignment(=)
      subroutine myAsgn(a,b)
         import base, child
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in)  :: b
      end subroutine
   end interface

   class(base(4)), allocatable :: b1, b2, b3, b4

end module

program userDefOp001kl
   use m

   open ( 1, file='userDefOp001kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = child(4,4) ( 1000, 1001 ) )
   allocate ( b2, source = child(4,4) ( 2000, 2002 ) )
   allocate ( child(4,4) :: b3 )

   b3 = b1 + b2

   deallocate ( b1, b2 )

   allocate ( b1, source = base(4) ( 100 ) )
   allocate ( b2, source = base(4) ( 200 ) )
   allocate ( base(4) :: b4 )

   b4 = b1 + b2

end program


class(base(4)) function myAdd(a,b) ! tcx: (4)
   use m, only: base, child

   interface write(formatted)
      subroutine writeFormatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(200) :: msg
   class(base(4)), intent(in) :: a, b
   allocatable :: myAdd

   if ( .not. same_type_as ( a, b ) ) error stop 1_4

   select type ( a )
      type is ( base(4) )
         allocate ( myAdd, source = base(4) ( i=(a%i+b%i) ) )
      type is ( child(4,4) )
         select type ( b )
            type is ( child(4,4) )
               allocate ( myAdd, source = child(4,4) ( i=(a%i+b%i), j=(a%j+b%j) ) )
            class default
               error stop 2_4
         end select
   end select

   write ( 1, "(DT(5,5),' = ',DT(6,6),' + ',DT(7,7))", iostat = stat, iomsg = msg )   myAdd, a, b
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end function

subroutine myAsgn(a,b)
   use m, only: base, child
   class(base(4)), intent(out) :: a
   class(base(4)), intent(in)  :: b
   integer :: stat
   character(200) :: msg

   interface write(formatted)
      subroutine writeFormatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   if ( .not. same_type_as ( a, b ) ) error stop 4_4

   select type ( b )
      type is (base(4))
        a%i = b%i
      type is (child(4,4))
        select type ( a )
           type is ( child(4,4) )
              a%i = b%i
              a%j = b%j
           class default
              error stop 5_4
        end select
   end select

   write ( 1, "(DT(5,5),' = ',DT(6,6))", iostat = stat, iomsg = msg )   a, b
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 6_4

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   select type ( dtv )
      type is ( base(4) )
         write ( fmt, "(A2,I1,A1)" ) '(I', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child(4,4) )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',I',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
