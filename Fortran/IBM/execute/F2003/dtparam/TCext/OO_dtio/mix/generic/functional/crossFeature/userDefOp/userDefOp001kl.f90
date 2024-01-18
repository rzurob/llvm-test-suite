!*  ===================================================================
!*
!*  TEST CASE NAME             : userDefOp001kl
!*
!*  DATE                       : 2007-08-08 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: GENERIC BINDING
!*                                        User Defined Operator: ensure DTIO can be invoked inside
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

   type base (kbase1) ! kbase1=4
      integer, kind :: kbase1
      integer(kbase1) :: i
      contains
         procedure, pass :: write => writeb
         generic :: write(formatted) => write
   end type

   type, extends(base) :: child (kchild1) ! kchild1=4
      integer, kind :: kchild1
      integer(kchild1) :: j
      contains
         procedure, pass :: write => writec
   end type

   interface operator(+)
      class(base(4)) function myAdd(a,b) ! tcx: (4)
         import base, child
         class(base(4)), intent(in) :: a, b ! tcx: (4)
         allocatable :: myAdd
      end function
   end interface

   interface assignment(=)
      subroutine myAsgn(a,b)
         import base, child
         class(base(4)), intent(out) :: a ! tcx: (4)
         class(base(4)), intent(in)  :: b ! tcx: (4)
      end subroutine
   end interface

   class(base(4)), allocatable :: b1, b2, b3, b4 ! tcx: (4)

   contains

   subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
      class(base(4)), intent(in) :: dtv ! tcx: (4)
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in)     :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      character(25) :: fmt

      write ( fmt, "(A2,I1,A1)" ) '(I', v_list(1),')'
      write ( unit, fmt, iostat = iostat )    dtv%i
      iomsg = 'dtiowriteb'

   end subroutine

   subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
      class(child(4,4)), intent(in) :: dtv ! tcx: (4,4)
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in)     :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      character(25) :: fmt

      write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',I',v_list(2),')'
      write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j

      iomsg = 'dtiowritec'

   end subroutine

end module

program userDefOp001kl
   use m

   open ( 1, file='userDefOp001kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = child(4,4) ( 1000, 1001 ) ) ! tcx: (4,4)
   allocate ( b2, source = child(4,4) ( 2000, 2002 ) ) ! tcx: (4,4)
   allocate ( child(4,4) :: b3 ) ! tcx: (4,4)

   b3 = b1 + b2

   deallocate ( b1, b2 )

   allocate ( b1, source = base(4) ( 100 ) ) ! tcx: (4)
   allocate ( b2, source = base(4) ( 200 ) ) ! tcx: (4)
   allocate ( base(4) :: b4 ) ! tcx: (4)

   b4 = b1 + b2

end program

class(base(4)) function myAdd(a,b) ! tcx: (4)
   use m, only: base, child

   integer :: stat
   character(200) :: msg
   class(base(4)), intent(in) :: a, b ! tcx: (4)
   allocatable :: myAdd

   if ( .not. same_type_as ( a, b ) ) error stop 1_4

   select type ( a )
      type is ( base(4) ) ! tcx: (4)
         allocate ( myAdd, source = base(4) ( i=(a%i+b%i) ) ) ! tcx: (4)
      type is ( child(4,4) ) ! tcx: (4,4)
         select type ( b )
            type is ( child(4,4) ) ! tcx: (4,4)
               allocate ( myAdd, source = child(4,4) ( i=(a%i+b%i), j=(a%j+b%j) ) ) ! tcx: (4,4)
            class default
               error stop 2_4
         end select
   end select

   write ( 1, "(DT(5,5),' = ',DT(6,6),' + ',DT(7,7))", iostat = stat, iomsg = msg )   myAdd, a, b

end function

subroutine myAsgn(a,b)
   use m, only: base, child
   class(base(4)), intent(out) :: a ! tcx: (4)
   class(base(4)), intent(in)  :: b ! tcx: (4)
   integer :: stat
   character(200) :: msg

   if ( .not. same_type_as ( a, b ) ) error stop 4_4

   select type ( b )
      type is (base(4)) ! tcx: (4)
        a%i = b%i
      type is (child(4,4)) ! tcx: (4,4)
        select type ( a )
           type is ( child(4,4) ) ! tcx: (4,4)
              a%i = b%i
              a%j = b%j
           class default
              error stop 5_4
        end select
   end select

   write ( 1, "(DT(5,5),' = ',DT(6,6))", iostat = stat, iomsg = msg )   a, b

end subroutine




! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase1) to invoke with (4) / declare with (4)! type: base - added parameters (kbase1) to invoke with (4) / declare with (4)! type: base - added parameters (kbase1) to invoke with (4) / declare with (4) - 16 changes
! type: child - added parameters (kchild1) to invoke with (4,4) / declare with (4,4) - 9 changes
