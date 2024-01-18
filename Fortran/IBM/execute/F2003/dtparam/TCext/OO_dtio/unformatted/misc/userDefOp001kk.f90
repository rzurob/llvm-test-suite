! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : userDefOp001kk
!*
!*  PROGRAMMER                 : David Forster (derived from userDefOp001 by Robert Ma)
!*  DATE                       : 2007-09-10 (original: 09/28/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing:  User-defined operator and assignment with DTIO
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type :: base (kbase_1) ! kbase_1=4
      integer, kind :: kbase_1
      integer(kbase_1) :: i1
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1)   :: i2
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

end module

program userDefOp001kk
   use m

   class(base(4)), allocatable :: b1, b2, b3, b4 ! tcx: (4)
   character(16) :: c1, c4
   character(1) :: c2, c3, c5, c6
   integer(4) :: i1, i2, i3
   integer(4) :: i4(2), i5(2), i6(2)

   open ( 1, file='userDefOp001kk.1', form='unformatted', access='sequential' )

   allocate ( b1, source = child(4,4) ( 1000, 1001 ) ) ! tcx: (4,4)
   allocate ( b2, source = child(4,4) ( 2000, 2001 ) ) ! tcx: (4,4)

   allocate ( child(4,4) :: b3 ) ! tcx: (4,4)

   b3 = b1 + b2

   deallocate ( b1, b2 )

   allocate ( b1, source = base(4) ( 100 ) ) ! tcx: (4)
   allocate ( b2, source = base(4) ( 200 ) ) ! tcx: (4)

   allocate ( base(4) :: b4 ) ! tcx: (4)

   b4 = b1 + b2

   rewind 1

   read (1) c4, i4, c5, i5, c6, i6
   read (1) c1, i1, c2, i2, c3, i3

   print *, c4, i4, c5, i5, c6, i6
   print *, c1, i1, c2, i2, c3, i3

   close ( 1, status = 'delete' )

end program


class(base(4)) function myAdd(a,b) ! tcx: (4)
   use m, only: base, child

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(200) :: msg
   class(base(4)), intent(in) :: a, b ! tcx: (4)
   allocatable :: myAdd

   if ( .not. same_type_as ( a, b ) ) error stop 101_4

   select type ( a )
      type is ( base(4) ) ! tcx: (4)
         allocate ( myAdd, source = base(4) ( i1= ( a%i1+b%i1 ) ) ) ! tcx: (4)
         write ( 1, iostat = stat, iomsg = msg ) "MyAdd = A + B : ", MyAdd, "=", A, "+", B
      type is ( child(4,4) ) ! tcx: (4,4)
         select type ( b )
            type is ( child(4,4) ) ! tcx: (4,4)
               allocate ( myAdd, source = child(4,4) ( i1= ( a%i1+b%i1 ), i2= ( a%i2+b%i2 ) ) ) ! tcx: (4,4)
               write ( 1, iostat = stat, iomsg = msg ) "MyAdd = A + B : ", MyAdd, "=", A, "+", B
            class default
               error stop 2_4
         end select
   end select

end function

subroutine myAsgn(a,b)
   use m, only: base, child
   class(base(4)), intent(out) :: a ! tcx: (4)
   class(base(4)), intent(in)  :: b ! tcx: (4)

   if ( .not. same_type_as ( a, b ) ) error stop 3_4

   select type ( b )
      type is (base(4)) ! tcx: (4)
        a%i1 = b%i1
      type is (child(4,4)) ! tcx: (4,4)
        select type ( a )
           type is ( child(4,4) ) ! tcx: (4,4)
              a%i1 = b%i1
              a%i2 = b%i2
           class default
              error stop 4_4
        end select
   end select

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m
   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base(4)) ! tcx: (4)
         write (unit, iostat=iostat, iomsg=iomsg ) dtv%i1
      type is (child(4,4)) ! tcx: (4,4)
         write (unit, iostat=iostat, iomsg=iomsg ) dtv%i1, dtv%i2
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 18 changes
! type: child - added parameters (kchild_1) to invoke with (4,4) / declare with (4,4) - 9 changes
